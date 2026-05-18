#' =============================================================================
#' Pattern Blocks — Geradores de Blocos Base
#' =============================================================================
#' Cada função retorna uma estrutura padronizada:
#'   $points    : lista nomeada de pontos c(x,y)
#'   $curves    : lista nomeada de data.frames (x,y)  
#'   $seams     : lista de segmentos (from, to) para costura
#'   $piece     : nome da peça ("front", "back", "sleeve", etc.)
#'   $area      : área aproximada em cm² e m²
#'   $metadata  : medidas e ease usados
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_measurements.R"))

#' Criar estrutura padrão de retorno para uma peça
create_piece <- function(name, points, curves, seams, metadata) {
  # Calcular área automaticamente
  if (length(seams) > 0) {
    outline_points <- get_outline_points(points, seams)
    area_cm2 <- polygon_area(outline_points$x, outline_points$y)
  } else {
    area_cm2 <- 0
  }
  
  list(
    piece = name,
    points = points,
    curves = curves,
    seams = seams,
    area = list(
      area_cm2 = area_cm2,
      area_m2 = area_cm2 / 10000
    ),
    metadata = metadata
  )
}

#' Extrair pontos do contorno na ordem correta para cálculo de área
get_outline_points <- function(points, seams) {
  if (length(seams) == 0) return(data.frame(x = numeric(0), y = numeric(0)))
  
  x <- numeric(0)
  y <- numeric(0)
  
  for (seam in seams) {
    if (!is.null(seam$from)) {
      pt <- points[[seam$from]]
      if (!is.null(pt)) {
        x <- c(x, pt[1])
        y <- c(y, pt[2])
      }
    }
  }
  
  # Fechar o polígono
  if (length(x) > 0) {
    x <- c(x, x[1])
    y <- c(y, y[1])
  }
  
  data.frame(x = x, y = y)
}

# =============================================================================
# SOPHIA JOBIM — Blusa (Corpo Simples)
# =============================================================================

#' Draft Bodice Front — Sophia Jobim
draft_bodice_front_sj <- function(medidas, ease, armscye_depth = NULL) {
  
  # Extrair medidas
  measure_bust <- m_get(medidas, "bust_circ")
  measure_front_length <- m_get(medidas, "neck_front_to_waist_f")
  measure_arm_circ <- m_get(medidas, "armscye_circ")
  
  # Extrair parâmetros
  ease_bust <- e_get(ease, "bust_ease")
  offset_AE <- e_get(ease, "side_seam_offset")
  offset_JM <- e_get(ease, "shoulder_slope_front")
  offset_AN_extra <- e_get(ease, "front_neckline_depth_extra")
  offset_neckline_shift <- e_get(ease, "front_neckline_shift")
  offset_R_bisector <- e_get(ease, "bissetriz_R")
  offset_VY <- e_get(ease, "desvio_V_esquerda")
  offset_HW <- e_get(ease, "subida_inferior")
  offset_Wcd <- e_get(ease, "desvio_W_lateral")
  
  # Profundidade da cava
  if (is.null(armscye_depth)) {
    armscye_depth <- get_armscye_depth(measure_arm_circ)
  }
  
  # Dimensões do retângulo
  dim_width <- (measure_bust + ease_bust) / 2
  dim_height <- measure_front_length
  
  # Pontos base
  point_A <- c(0, dim_height)
  point_D <- c(0, 0)
  point_E <- c(offset_AE, dim_height)
  point_F <- c(offset_AE, 0)
  point_G <- c((point_E[1] + dim_width) / 2, dim_height)
  point_H <- c((point_F[1] + dim_width) / 2, 0)
  
  # Divisões da frente
  dim_front_division <- (point_G[1] - point_A[1]) / 4
  point_I <- c(point_A[1] + 1 * dim_front_division, dim_height)
  point_J <- c(point_A[1] + 3 * dim_front_division, dim_height)
  
  # Ombro frente
  point_M <- c(point_J[1], point_J[2] - offset_JM)
  
  # Decote frente
  distance_AI <- point_I[1] - point_A[1]
  dim_front_neck_depth <- distance_AI + offset_AN_extra
  point_N <- c(point_A[1], point_A[2] - dim_front_neck_depth)
  
  angle_IM <- atan2(point_M[2] - point_I[2], point_M[1] - point_I[1])
  point_I2 <- c(
    point_I[1] + offset_neckline_shift * cos(angle_IM),
    point_I[2] + offset_neckline_shift * sin(angle_IM)
  )
  
  # Curva decote frente
  mid_N_I2 <- midpoint(point_N, point_I2)
  control_N_I2 <- c(mid_N_I2[1] - 1, mid_N_I2[2] - 3)
  curve_neckline_front <- create_curve(
    x = c(point_N[1], control_N_I2[1], point_I2[1]),
    y = c(point_N[2], control_N_I2[2], point_I2[2]),
    n = 50
  )
  
  # Cava (parte da frente)
  point_R <- c(point_J[1], point_J[2] - armscye_depth)
  point_Q <- c(point_G[1], point_R[2])
  point_T <- c(
    point_R[1] + offset_R_bisector * cos(pi/4),
    point_R[2] + offset_R_bisector * sin(pi/4)
  )
  point_V <- midpoint(point_M, point_R)
  point_Y <- c(point_V[1] - offset_VY, point_V[2])
  
  # Parte inferior frente
  point_W <- c(point_H[1], point_H[2] + offset_HW)
  point_b <- c(point_I[1], 0)
  point_c <- c(point_W[1] - offset_Wcd, point_W[2])
  
  # Coletar pontos
  points <- list(
    point_A = point_A, point_D = point_D,
    point_I = point_I, point_J = point_J,
    point_M = point_M, point_N = point_N,
    point_I2 = point_I2,
    point_Q = point_Q, point_R = point_R,
    point_T = point_T, point_V = point_V, point_Y = point_Y,
    point_W = point_W, point_b = point_b, point_c = point_c
  )
  
  # Definir costuras (ordem do contorno)
  seams <- list(
    list(from = "point_I", to = "point_M", name = "shoulder"),
    list(from = "point_M", to = "point_Y", name = "armscye_upper"),
    list(from = "point_Y", to = "point_T", name = "armscye_mid"),
    list(from = "point_T", to = "point_Q", name = "armscye_lower"),
    list(from = "point_Q", to = "point_c", name = "side_seam"),
    list(from = "point_c", to = "point_b", name = "waist"),
    list(from = "point_b", to = "point_D", name = "hem"),
    list(from = "point_D", to = "point_N", name = "center_front"),
    list(from = "point_N", to = "point_I2", name = "neckline"),
    list(from = "point_I2", to = "point_I", name = "neckline_shift")
  )
  
  # Curvas
  curves <- list(
    curve_neckline_front = curve_neckline_front
  )
  
  # Metadados
  metadata <- list(
    method = "Sophia Jobim",
    piece = "Bodice Front",
    measurements = list(
      bust = measure_bust,
      front_length = measure_front_length
    ),
    ease = list(
      bust_ease = ease_bust,
      offset_AE = offset_AE,
      offset_JM = offset_JM,
      armscye_depth = armscye_depth
    ),
    dimensions = list(
      width = dim_width,
      height = dim_height,
      front_division = dim_front_division,
      neck_depth = dim_front_neck_depth
    )
  )
  
  create_piece("front", points, curves, seams, metadata)
}

#' Draft Bodice Back — Sophia Jobim
draft_bodice_back_sj <- function(medidas, ease, armscye_depth = NULL) {
  
  # Extrair medidas
  measure_bust <- m_get(medidas, "bust_circ")
  measure_front_length <- m_get(medidas, "neck_front_to_waist_f")
  measure_arm_circ <- m_get(medidas, "armscye_circ")
  
  # Extrair parâmetros
  ease_bust <- e_get(ease, "bust_ease")
  offset_AE <- e_get(ease, "side_seam_offset")
  offset_neckline_shift <- e_get(ease, "front_neckline_shift")
  offset_B_neck <- e_get(ease, "back_neck_drop")
  offset_KO <- e_get(ease, "back_dart_height")
  offset_LO_ext <- e_get(ease, "back_shoulder_extension")
  offset_S_bisector <- e_get(ease, "bissetriz_S")
  offset_XZ <- e_get(ease, "desvio_X_esquerda")
  offset_HW <- e_get(ease, "subida_inferior")
  offset_Wcd <- e_get(ease, "desvio_W_lateral")
  
  # Profundidade da cava
  if (is.null(armscye_depth)) {
    armscye_depth <- get_armscye_depth(measure_arm_circ)
  }
  
  # Dimensões
  dim_width <- (measure_bust + ease_bust) / 2
  dim_height <- measure_front_length
  
  # Pontos base
  point_B <- c(dim_width, dim_height)
  point_C <- c(dim_width, 0)
  point_E <- c(offset_AE, dim_height)
  point_F <- c(offset_AE, 0)
  point_G <- c((point_E[1] + dim_width) / 2, dim_height)
  point_H <- c((point_F[1] + dim_width) / 2, 0)
  
  # Divisões das costas
  dim_back_division <- (point_B[1] - point_G[1]) / 4
  point_K <- c(point_G[1] + 1 * dim_back_division, dim_height)
  point_L <- c(point_G[1] + 3 * dim_back_division, dim_height)
  
  # Ombro costas
  point_O <- c(point_K[1], point_K[2] - offset_KO)
  angle_LO <- atan2(point_O[2] - point_L[2], point_O[1] - point_L[1])
  point_P <- c(
    point_O[1] + offset_LO_ext * cos(angle_LO),
    point_O[2] + offset_LO_ext * sin(angle_LO)
  )
  
  # Decote costas
  point_B_neck <- c(point_B[1], point_B[2] - offset_B_neck)
  angle_LP <- atan2(point_P[2] - point_L[2], point_P[1] - point_L[1])
  point_L2 <- c(
    point_L[1] + offset_neckline_shift * cos(angle_LP),
    point_L[2] + offset_neckline_shift * sin(angle_LP)
  )
  
  mid_B_L2 <- midpoint(point_B_neck, point_L2)
  control_B_L2 <- c(mid_B_L2[1] + 1, mid_B_L2[2] - 2)
  curve_neckline_back <- create_curve(
    x = c(point_B_neck[1], control_B_L2[1], point_L2[1]),
    y = c(point_B_neck[2], control_B_L2[2], point_L2[2]),
    n = 50
  )
  
  # Cava (parte das costas)
  point_S <- c(point_K[1], point_K[2] - armscye_depth)
  point_Q <- c(point_G[1], point_S[2])
  point_U <- c(
    point_S[1] + offset_S_bisector * cos(3*pi/4),
    point_S[2] + offset_S_bisector * sin(3*pi/4)
  )
  point_X <- midpoint(point_O, point_S)
  point_Z <- c(point_X[1] - offset_XZ, point_X[2])
  
  # Parte inferior costas
  point_W <- c(point_H[1], point_H[2] + offset_HW)
  point_a <- c(point_C[1], point_C[2] + offset_HW)
  point_d <- c(point_W[1] + offset_Wcd, point_W[2])
  
  # Coletar pontos
  points <- list(
    point_B = point_B, point_C = point_C,
    point_G = point_G,
    point_K = point_K, point_L = point_L,
    point_O = point_O, point_P = point_P,
    point_B_neck = point_B_neck, point_L2 = point_L2,
    point_Q = point_Q, point_S = point_S,
    point_U = point_U, point_X = point_X, point_Z = point_Z,
    point_W = point_W, point_a = point_a, point_d = point_d
  )
  
  # Costuras
  seams <- list(
    list(from = "point_L", to = "point_P", name = "shoulder"),
    list(from = "point_P", to = "point_Z", name = "armscye_upper"),
    list(from = "point_Z", to = "point_U", name = "armscye_mid"),
    list(from = "point_U", to = "point_Q", name = "armscye_lower"),
    list(from = "point_Q", to = "point_d", name = "side_seam"),
    list(from = "point_d", to = "point_a", name = "waist"),
    list(from = "point_a", to = "point_B_neck", name = "center_back"),
    list(from = "point_B_neck", to = "point_L2", name = "neckline"),
    list(from = "point_L2", to = "point_L", name = "neckline_shift")
  )
  
  # Curvas
  curves <- list(
    curve_neckline_back = curve_neckline_back
  )
  
  # Metadados
  metadata <- list(
    method = "Sophia Jobim",
    piece = "Bodice Back",
    measurements = list(
      bust = measure_bust,
      front_length = measure_front_length
    ),
    ease = list(
      bust_ease = ease_bust,
      offset_AE = offset_AE,
      armscye_depth = armscye_depth
    ),
    dimensions = list(
      width = dim_width,
      height = dim_height,
      back_division = dim_back_division
    )
  )
  
  create_piece("back", points, curves, seams, metadata)
}

#' Draft Complete Blouse — Sophia Jobim
draft_blouse_sj <- function(medidas, ease, armscye_depth = NULL) {
  
  # Gerar frente e costas separadamente
  front <- draft_bodice_front_sj(medidas, ease, armscye_depth)
  back <- draft_bodice_back_sj(medidas, ease, armscye_depth)
  
  # Gerar cava completa (junta os pontos das duas peças)
  cava_points <- list(
    P = back$points$point_P,
    Z = back$points$point_Z,
    U = back$points$point_U,
    Q = back$points$point_Q,
    T = front$points$point_T,
    Y = front$points$point_Y,
    M = front$points$point_M
  )
  
  cava_x <- sapply(cava_points, `[`, 1)
  cava_y <- sapply(cava_points, `[`, 2)
  
  cava_t <- c(0, cumsum(sqrt(diff(cava_x)^2 + diff(cava_y)^2)))
  unique_idx <- !duplicated(cava_t)
  
  curve_armscye <- data.frame(
    x = spline(cava_t[unique_idx], cava_x[unique_idx], method = "natural", n = 150)$y,
    y = spline(cava_t[unique_idx], cava_y[unique_idx], method = "natural", n = 150)$y
  )
  
  # Juntar tudo
  all_points <- c(front$points, back$points)
  all_curves <- c(front$curves, back$curves, list(curve_armscye = curve_armscye))
  
  # Área total
  total_area <- front$area$area_cm2 + back$area$area_cm2
  
  # Comprimentos das linhas
  line_lengths <- list(
    shoulder_front_IM = calc_distance(front$points$point_I, front$points$point_M),
    neckline_front = curve_length(front$curves$curve_neckline_front),
    shoulder_back_LP = calc_distance(back$points$point_L, back$points$point_P),
    neckline_back = curve_length(back$curves$curve_neckline_back),
    armscye = curve_length(curve_armscye),
    side_front_Qc = calc_distance(front$points$point_Q, front$points$point_c),
    side_back_Qd = calc_distance(back$points$point_Q, back$points$point_d),
    waist_front_cb = calc_distance(front$points$point_c, front$points$point_b),
    waist_back_da = calc_distance(back$points$point_d, back$points$point_a),
    hem_front_bD = calc_distance(front$points$point_b, front$points$point_D),
    center_front_DN = calc_distance(front$points$point_D, front$points$point_N),
    center_back_aB = calc_distance(back$points$point_a, back$points$point_B_neck)
  )
  
  list(
    front = front,
    back = back,
    points = all_points,
    curves = all_curves,
    line_lengths = line_lengths,
    area = list(
      front_area_cm2 = front$area$area_cm2,
      back_area_cm2 = back$area$area_cm2,
      total_area_cm2 = total_area,
      front_area_m2 = front$area$area_m2,
      back_area_m2 = back$area$area_m2,
      total_area_m2 = total_area / 10000,
      note = "Area aproximada. Adicionar 10-15% para margens e curvas."
    ),
    dimensions = list(
      dim_width = front$metadata$dimensions$width,
      dim_height = front$metadata$dimensions$height,
      dim_front_division = front$metadata$dimensions$front_division,
      dim_back_division = back$metadata$dimensions$back_division
    ),
    measurements = front$metadata$measurements,
    ease = front$metadata$ease
  )
}

#' Obter profundidade da cava da tabela Sophia Jobim
get_armscye_depth <- function(arm_circ) {
  tabela_cava <- readr::read_csv(
    here("data", "measurements", "sophia_jobim_armscye_table.csv"),
    show_col_types = FALSE
  )
  
  depth <- tabela_cava$armscye_depth[tabela_cava$arm_circ == arm_circ]
  
  if (length(depth) == 0) {
    depth <- approx(tabela_cava$arm_circ, tabela_cava$armscye_depth, 
                    xout = arm_circ)$y
  }
  
  depth
}