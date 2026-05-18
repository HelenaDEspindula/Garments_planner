#' =============================================================================
#' Pattern Modifiers — Modificações de Peças Base
#' =============================================================================
#' Cada função recebe uma peça (saída de pattern_blocks) e retorna
#' a peça modificada no mesmo formato.
#' 
#' Pipeline típico:
#'   peça %>% add_bust_dart() %>% add_waist_darts() %>% add_seam_allowance()
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))

#' Adicionar pence de busto na peça da frente
#' 
#' @param piece Peça da frente (de draft_bodice_front_*)
#' @param dart_width Largura da pence na lateral (cm). Default: 3
#' @param dart_length Comprimento da pence até o bust point (cm). Default: 10
#' @param bust_point Posição do bust point c(x, y). Se NULL, calcula automaticamente
#' @return Peça modificada
add_bust_dart <- function(piece, dart_width = 3, dart_length = 10, bust_point = NULL) {
  
  if (piece$piece != "front") {
    warning("Bust dart only applies to front piece")
    return(piece)
  }
  
  # Posição do bust point (se não fornecido, usa posição típica)
  if (is.null(bust_point)) {
    # Aproximadamente na altura do ponto R (cava) e na metade da distância
    # entre o centro frente e a lateral
    point_R <- piece$points$point_R
    point_D <- piece$points$point_D
    
    if (!is.null(point_R) && !is.null(point_D)) {
      bust_x <- point_D[1] + (point_R[1] - point_D[1]) * 0.4
      bust_y <- point_R[2] + (point_D[2] - point_R[2]) * 0.3
      bust_point <- c(bust_x, bust_y)
    } else {
      warning("Cannot determine bust point automatically")
      return(piece)
    }
  }
  
  # Encontrar o ponto Q (fundo da cava) para abrir a pence na lateral
  point_Q <- piece$points$point_Q
  point_c <- piece$points$point_c
  
  if (is.null(point_Q) || is.null(point_c)) {
    warning("Required points Q and c not found")
    return(piece)
  }
  
  # Criar ponto de abertura da pence na costura lateral (entre Q e c)
  dart_opening <- midpoint(point_Q, point_c)
  
  # Criar ponto de controle para a ponta da pence
  angle_to_bust <- calc_angle(dart_opening, bust_point)
  
  # Ponto da ponta da pence (no comprimento especificado)
  dart_tip <- c(
    bust_point[1] + (dart_length - calc_distance(dart_opening, bust_point)) * cos(angle_to_bust),
    bust_point[2] + (dart_length - calc_distance(dart_opening, bust_point)) * sin(angle_to_bust)
  )
  
  # Criar pernas da pence na costura lateral
  half_width <- dart_width / 2
  
  # Vetor perpendicular à linha Q-c
  angle_QC <- calc_angle(point_Q, point_c)
  perp_angle <- angle_QC + pi/2
  
  dart_leg_upper <- c(
    dart_opening[1] + half_width * cos(perp_angle),
    dart_opening[2] + half_width * sin(perp_angle)
  )
  
  dart_leg_lower <- c(
    dart_opening[1] - half_width * cos(perp_angle),
    dart_opening[2] - half_width * sin(perp_angle)
  )
  
  # Adicionar novos pontos
  piece$points$dart_tip <- dart_tip
  piece$points$dart_leg_upper <- dart_leg_upper
  piece$points$dart_leg_lower <- dart_leg_lower
  
  # Modificar as costuras: Q -> dart_leg_upper -> dart_tip -> dart_leg_lower -> c
  piece$seams <- modify_seams_for_dart(
    piece$seams, 
    "point_Q", "point_c",
    "dart_leg_upper", "dart_tip", "dart_leg_lower"
  )
  
  # Atualizar metadados
  piece$metadata$modifications <- c(
    piece$metadata$modifications,
    list(bust_dart = list(
      width = dart_width,
      length = dart_length,
      bust_point = bust_point
    ))
  )
  
  # Recalcular área
  piece$area <- recalculate_area(piece)
  
  piece
}

#' Adicionar pences de cintura
#' 
#' @param piece Peça (frente ou costas)
#' @param dart_width Largura total da pence (cm). Default: 2
#' @param dart_length Comprimento da pence (cm). Default: 8 para frente, 10 para costas
#' @return Peça modificada
add_waist_dart <- function(piece, dart_width = 2, dart_length = NULL) {
  
  # Determinar comprimento padrão por peça
  if (is.null(dart_length)) {
    dart_length <- if (piece$piece == "front") 8 else 10
  }
  
  # Encontrar a costura da cintura
  waist_seam <- NULL
  for (seam in piece$seams) {
    if (seam$name == "waist") {
      waist_seam <- seam
      break
    }
  }
  
  if (is.null(waist_seam)) {
    warning("Waist seam not found")
    return(piece)
  }
  
  # Ponto médio da cintura
  waist_from <- piece$points[[waist_seam$from]]
  waist_to <- piece$points[[waist_seam$to]]
  waist_mid <- midpoint(waist_from, waist_to)
  
  # Direção para cima (perpendicular à cintura)
  angle_waist <- calc_angle(waist_from, waist_to)
  upward_angle <- angle_waist + pi/2
  
  # Ponta da pence
  dart_tip <- c(
    waist_mid[1] + dart_length * cos(upward_angle),
    waist_mid[2] + dart_length * sin(upward_angle)
  )
  
  # Pernas da pence
  half_width <- dart_width / 2
  
  dart_leg_left <- c(
    waist_mid[1] - half_width * cos(angle_waist),
    waist_mid[2] - half_width * sin(angle_waist)
  )
  
  dart_leg_right <- c(
    waist_mid[1] + half_width * cos(angle_waist),
    waist_mid[2] + half_width * sin(angle_waist)
  )
  
  # Adicionar pontos
  prefix <- if (piece$piece == "front") "waist" else "back_waist"
  piece$points[[paste0(prefix, "_dart_tip")]] <- dart_tip
  piece$points[[paste0(prefix, "_dart_leg_left")]] <- dart_leg_left
  piece$points[[paste0(prefix, "_dart_leg_right")]] <- dart_leg_right
  
  # Modificar costura
  piece$seams <- modify_seams_for_dart(
    piece$seams,
    waist_seam$from, waist_seam$to,
    paste0(prefix, "_dart_leg_left"),
    paste0(prefix, "_dart_tip"),
    paste0(prefix, "_dart_leg_right")
  )
  
  # Recalcular área
  piece$area <- recalculate_area(piece)
  
  piece
}

#' Modificar decote (profundidade e/ou largura)
#' 
#' @param piece Peça (frente ou costas)
#' @param depth_change Mudança na profundidade (cm, positivo = mais fundo)
#' @param width_change Mudança na largura (cm, positivo = mais largo)
#' @return Peça modificada
modify_neckline <- function(piece, depth_change = 0, width_change = 0) {
  
  if (depth_change == 0 && width_change == 0) return(piece)
  
  # Encontrar ponto do decote
  neck_point <- NULL
  if (piece$piece == "front") {
    neck_point <- piece$points$point_N
  } else {
    neck_point <- piece$points$point_B_neck
  }
  
  if (is.null(neck_point)) {
    warning("Neck point not found")
    return(piece)
  }
  
  # Ajustar profundidade (y)
  if (depth_change != 0) {
    neck_point[2] <- neck_point[2] - depth_change
    
    if (piece$piece == "front") {
      piece$points$point_N <- neck_point
    } else {
      piece$points$point_B_neck <- neck_point
    }
  }
  
  # Ajustar largura (x) - move ponto do ombro
  if (width_change != 0) {
    shoulder_point <- if (piece$piece == "front") {
      piece$points$point_I
    } else {
      piece$points$point_L
    }
    
    if (!is.null(shoulder_point)) {
      direction <- if (piece$piece == "front") 1 else -1
      shoulder_point[1] <- shoulder_point[1] + direction * width_change
      
      if (piece$piece == "front") {
        piece$points$point_I <- shoulder_point
      } else {
        piece$points$point_L <- shoulder_point
      }
    }
  }
  
  # Recalcular área
  piece$area <- recalculate_area(piece)
  
  piece
}

#' Adicionar margem de costura a todas as bordas
#' 
#' @param piece Peça
#' @param allowance Margem em cm. Default: 1.5
#' @return Peça modificada com novos pontos de margem
add_seam_allowance <- function(piece, allowance = 1.5) {
  
  # Para cada ponto do contorno, criar ponto offset
  # (Implementação simplificada - versão completa usaria offset de polígono)
  
  for (name in names(piece$points)) {
    pt <- piece$points[[name]]
    # Ponto de margem (aproximação: desloca para fora)
    piece$points[[paste0(name, "_sa")]] <- c(pt[1] + allowance, pt[2] + allowance)
  }
  
  piece$metadata$seam_allowance <- allowance
  piece
}

# =============================================================================
# Funções auxiliares internas
# =============================================================================

#' Modificar lista de costuras para incluir uma pence
modify_seams_for_dart <- function(seams, from_point, to_point, 
                                  leg1, tip, leg2) {
  
  new_seams <- list()
  
  for (seam in seams) {
    if (seam$from == from_point && seam$to == to_point) {
      # Substituir costura original por três segmentos da pence
      new_seams[[length(new_seams) + 1]] <- list(
        from = from_point, to = leg1, name = paste0(seam$name, "_pre_dart")
      )
      new_seams[[length(new_seams) + 1]] <- list(
        from = leg1, to = tip, name = paste0(seam$name, "_dart_leg1")
      )
      new_seams[[length(new_seams) + 1]] <- list(
        from = tip, to = leg2, name = paste0(seam$name, "_dart_leg2")
      )
      new_seams[[length(new_seams) + 1]] <- list(
        from = leg2, to = to_point, name = paste0(seam$name, "_post_dart")
      )
    } else {
      new_seams[[length(new_seams) + 1]] <- seam
    }
  }
  
  new_seams
}

#' Recalcular área da peça
recalculate_area <- function(piece) {
  outline_points <- get_outline_points(piece$points, piece$seams)
  area_cm2 <- polygon_area(outline_points$x, outline_points$y)
  
  list(
    area_cm2 = area_cm2,
    area_m2 = area_cm2 / 10000
  )
}