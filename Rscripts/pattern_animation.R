#' =============================================================================
#' Pattern Animation — Animações do Processo de Modelagem
#' =============================================================================
#' Formatos suportados:
#'   - GIF/MP4: Animação do traçado passo a passo
#'   - Manim: Exportação para animações de alta qualidade
#'   - Transição 2D→3D: Preparação para visualização tridimensional
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))

#' Criar animação do traçado passo a passo (GIF)
#'
#' @param steps Lista de estados intermediários do traçado
#' @param output_file Caminho do arquivo .gif
#' @param duration Duração total em segundos
#' @param width Largura em pixels
#' @param height Altura em pixels
#' @return Caminho do arquivo gerado (invisível)
animate_drafting <- function(steps, output_file, 
                             duration = 10, 
                             width = 800, 
                             height = 800) {
  
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("Package 'gifski' is required. Install with: install.packages('gifski')")
  }
  
  # Número de frames
  n_frames <- length(steps) * 10  # 10 frames por passo
  frame_duration <- duration / n_frames
  
  # Criar frames temporários
  temp_dir <- tempdir()
  frame_files <- character(n_frames)
  
  for (i in seq_len(n_frames)) {
    step_idx <- ceiling(i / 10)
    progress <- (i - (step_idx - 1) * 10) / 10  # 0 a 1 dentro do passo
    
    frame_file <- file.path(temp_dir, sprintf("frame_%04d.png", i))
    
    # Criar frame
    p <- create_frame(steps, step_idx, progress, width, height)
    
    ggsave(frame_file, p, width = width/100, height = height/100, 
           dpi = 100, device = "png")
    
    frame_files[i] <- frame_file
  }
  
  # Compilar GIF
  gifski::gifski(frame_files, output_file,
                 width = width, height = height,
                 delay = frame_duration,
                 progress = FALSE)
  
  # Limpar frames temporários
  unlink(frame_files)
  
  message("Animation saved: ", output_file)
  invisible(output_file)
}

#' Criar um frame da animação
create_frame <- function(steps, step_idx, progress, width, height) {
  
  # Determinar dimensões
  dim_width <- 55
  dim_height <- 50
  
  p <- ggplot() +
    coord_fixed(xlim = c(-5, dim_width), ylim = c(-5, dim_height)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  # Acumular elementos dos passos anteriores
  for (s in 1:step_idx) {
    step <- steps[[s]]
    alpha <- if (s < step_idx) 1 else progress
    
    # Adicionar pontos
    if (!is.null(step$points)) {
      for (pt in step$points) {
        p <- p + annotate("point", x = pt[1], y = pt[2],
                          size = 3, alpha = alpha, color = "#333333")
      }
    }
    
    # Adicionar linhas
    if (!is.null(step$lines)) {
      for (line in step$lines) {
        p <- p + annotate("segment",
                          x = line$from[1], xend = line$to[1],
                          y = line$from[2], yend = line$to[2],
                          color = line$color %||% "#333333",
                          linewidth = line$width %||% 1,
                          alpha = alpha)
      }
    }
    
    # Adicionar curvas
    if (!is.null(step$curves)) {
      for (curve in step$curves) {
        # Animar a curva crescendo
        n_points <- nrow(curve$data)
        visible_points <- max(1, floor(progress * n_points))
        
        curve_part <- curve$data[1:visible_points, ]
        p <- p + geom_path(data = curve_part, aes(x, y),
                           color = curve$color %||% "#333333",
                           linewidth = curve$width %||% 1.5,
                           alpha = alpha)
      }
    }
    
    # Adicionar labels
    if (!is.null(step$labels)) {
      for (label in step$labels) {
        p <- p + annotate("text", x = label$pos[1], y = label$pos[2],
                          label = label$text,
                          size = label$size %||% 4,
                          color = label$color %||% "#333333",
                          alpha = alpha)
      }
    }
  }
  
  # Título do passo atual
  current_step <- steps[[step_idx]]
  if (!is.null(current_step$title)) {
    p <- p + annotate("text", x = dim_width/2, y = dim_height + 2,
                      label = current_step$title,
                      size = 6, fontface = "bold", color = "#333333")
  }
  
  p
}

#' Criar sequência de passos para animação da blusa Sophia Jobim
#'
#' @param medidas Lista de medidas
#' @param ease Lista de parâmetros
#' @return Lista de passos para animate_drafting()
create_blouse_animation_steps <- function(medidas, ease) {
  
  source(here("Rscripts", "pattern_blocks.R"))
  
  # Gerar bloco completo
  blusa <- draft_blouse_sj(medidas, ease)
  
  # Extrair pontos
  pts <- blusa$points
  
  steps <- list()
  
  # Passo 1: Retângulo base
  steps[[1]] <- list(
    title = "Passo 1: Retângulo Base ABCD",
    points = list(
      pts$point_A, pts$point_B, pts$point_C, pts$point_D
    ),
    lines = list(
      list(from = pts$point_A, to = pts$point_B, color = "grey50", width = 0.5),
      list(from = pts$point_B, to = pts$point_C, color = "grey50", width = 0.5),
      list(from = pts$point_C, to = pts$point_D, color = "grey50", width = 0.5),
      list(from = pts$point_D, to = pts$point_A, color = "grey50", width = 0.5)
    ),
    labels = list(
      list(pos = pts$point_A, text = "A"),
      list(pos = pts$point_B, text = "B"),
      list(pos = pts$point_C, text = "C"),
      list(pos = pts$point_D, text = "D")
    )
  )
  
  # Passo 2: Deslocamento EF
  steps[[2]] <- list(
    title = "Passo 2: Deslocamento EF",
    points = list(pts$point_E, pts$point_F),
    lines = list(
      list(from = pts$point_E, to = pts$point_F, color = "#377EB8", width = 1)
    ),
    labels = list(
      list(pos = pts$point_E, text = "E"),
      list(pos = pts$point_F, text = "F")
    )
  )
  
  # Passo 3: Linha central GH
  steps[[3]] <- list(
    title = "Passo 3: Linha Central GH",
    points = list(pts$point_G, pts$point_H),
    lines = list(
      list(from = pts$point_G, to = pts$point_H, color = "#4DAF4A", width = 1)
    ),
    labels = list(
      list(pos = pts$point_G, text = "G"),
      list(pos = pts$point_H, text = "H")
    )
  )
  
  # Passo 4: Divisões
  steps[[4]] <- list(
    title = "Passo 4: Divisões I, J, K, L",
    points = list(pts$point_I, pts$point_J, pts$point_K, pts$point_L),
    labels = list(
      list(pos = pts$point_I, text = "I"),
      list(pos = pts$point_J, text = "J"),
      list(pos = pts$point_K, text = "K"),
      list(pos = pts$point_L, text = "L")
    )
  )
  
  # Passo 5: Ombro frente
  steps[[5]] <- list(
    title = "Passo 5: Ombro Frente (M)",
    points = list(pts$point_M),
    lines = list(
      list(from = pts$point_I, to = pts$point_M, color = "#2196F3", width = 1.5)
    ),
    labels = list(
      list(pos = pts$point_M, text = "M")
    )
  )
  
  # Passo 6: Decote frente
  steps[[6]] <- list(
    title = "Passo 6: Decote Frente",
    points = list(pts$point_N, pts$point_I2),
    curves = list(
      list(data = blusa$front$curves$curve_neckline_front, 
           color = "#2196F3", width = 1.5)
    ),
    labels = list(
      list(pos = pts$point_N, text = "N"),
      list(pos = pts$point_I2, text = "I2")
    )
  )
  
  # Passo 7: Decote costas
  steps[[7]] <- list(
    title = "Passo 7: Decote Costas",
    points = list(pts$point_B_neck, pts$point_L2),
    curves = list(
      list(data = blusa$back$curves$curve_neckline_back, 
           color = "#FF9800", width = 1.5)
    ),
    labels = list(
      list(pos = pts$point_B_neck, text = "B'"),
      list(pos = pts$point_L2, text = "L2")
    )
  )
  
  # Passo 8: Ombro costas
  steps[[8]] <- list(
    title = "Passo 8: Ombro Costas (O, P)",
    points = list(pts$point_O, pts$point_P),
    lines = list(
      list(from = pts$point_L, to = pts$point_P, color = "#FF9800", width = 1.5)
    ),
    labels = list(
      list(pos = pts$point_O, text = "O"),
      list(pos = pts$point_P, text = "P")
    )
  )
  
  # Passo 9: Linha da cava
  steps[[9]] <- list(
    title = "Passo 9: Linha da Cava (R, S, Q)",
    points = list(pts$point_R, pts$point_S, pts$point_Q),
    lines = list(
      list(from = pts$point_R, to = pts$point_S, color = "grey70", width = 0.5)
    ),
    labels = list(
      list(pos = pts$point_R, text = "R"),
      list(pos = pts$point_S, text = "S"),
      list(pos = pts$point_Q, text = "Q")
    )
  )
  
  # Passo 10: Bissetrizes
  steps[[10]] <- list(
    title = "Passo 10: Bissetrizes (T, U)",
    points = list(pts$point_T, pts$point_U),
    labels = list(
      list(pos = pts$point_T, text = "T"),
      list(pos = pts$point_U, text = "U")
    )
  )
  
  # Passo 11: Pontos médios
  steps[[11]] <- list(
    title = "Passo 11: Pontos Médios (V, X, Y, Z)",
    points = list(pts$point_V, pts$point_X, pts$point_Y, pts$point_Z),
    labels = list(
      list(pos = pts$point_V, text = "V"),
      list(pos = pts$point_X, text = "X"),
      list(pos = pts$point_Y, text = "Y"),
      list(pos = pts$point_Z, text = "Z")
    )
  )
  
  # Passo 12: Curva da cava
  steps[[12]] <- list(
    title = "Passo 12: Curva da Cava",
    curves = list(
      list(data = blusa$curves$curve_armscye, color = "#333333", width = 2)
    )
  )
  
  # Passo 13: Parte inferior
  steps[[13]] <- list(
    title = "Passo 13: Parte Inferior",
    points = list(pts$point_W, pts$point_a, pts$point_b, pts$point_c, pts$point_d),
    lines = list(
      list(from = pts$point_W, to = pts$point_a, color = "grey70", width = 0.5)
    ),
    labels = list(
      list(pos = pts$point_W, text = "W"),
      list(pos = pts$point_a, text = "a"),
      list(pos = pts$point_b, text = "b"),
      list(pos = pts$point_c, text = "c"),
      list(pos = pts$point_d, text = "d")
    )
  )
  
  # Passo 14: Conexões frente
  steps[[14]] <- list(
    title = "Passo 14: Conexões Frente",
    lines = list(
      list(from = pts$point_Q, to = pts$point_c, color = "#2196F3", width = 1.5),
      list(from = pts$point_c, to = pts$point_b, color = "#2196F3", width = 1.5),
      list(from = pts$point_b, to = pts$point_D, color = "#2196F3", width = 1.5),
      list(from = pts$point_D, to = pts$point_N, color = "#2196F3", width = 1.5)
    )
  )
  
  # Passo 15: Conexões costas
  steps[[15]] <- list(
    title = "Passo 15: Conexões Costas",
    lines = list(
      list(from = pts$point_Q, to = pts$point_d, color = "#FF9800", width = 1.5),
      list(from = pts$point_d, to = pts$point_a, color = "#FF9800", width = 1.5),
      list(from = pts$point_a, to = pts$point_B_neck, color = "#FF9800", width = 1.5)
    )
  )
  
  steps
}

#' Exportar dados para animação Manim (Python)
#'
#' @param steps Lista de passos (de create_blouse_animation_steps)
#' @param output_dir Diretório de saída
#' @return Caminho do arquivo gerado
export_for_manim <- function(steps, output_dir = "manim") {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Criar script Python para Manim
  python_script <- file.path(output_dir, "animate_blouse.py")
  
  sink(python_script)
  
  cat('from manim import *\n\n')
  cat('class BlouseDrafting(Scene):\n')
  cat('    def construct(self):\n')
  
  for (i in seq_along(steps)) {
    step <- steps[[i]]
    
    cat(sprintf('        # Step %d: %s\n', i, step$title))
    
    # Criar objetos
    if (!is.null(step$points)) {
      cat('        dots = VGroup()\n')
      for (j in seq_along(step$points)) {
        pt <- step$points[[j]]
        cat(sprintf('        dot_%d_%d = Dot(point=[%.2f, %.2f, 0])\n', 
                    i, j, pt[1]/10, pt[2]/10))
        cat(sprintf('        dots.add(dot_%d_%d)\n', i, j))
      }
      cat('        self.play(Create(dots))\n')
    }
    
    if (!is.null(step$lines)) {
      for (j in seq_along(step$lines)) {
        line <- step$lines[[j]]
        cat(sprintf('        line_%d_%d = Line(\n', i, j))
        cat(sprintf('            start=[%.2f, %.2f, 0],\n', 
                    line$from[1]/10, line$from[2]/10))
        cat(sprintf('            end=[%.2f, %.2f, 0]\n', 
                    line$to[1]/10, line$to[2]/10))
        cat('        )\n')
        cat(sprintf('        self.play(Create(line_%d_%d))\n', i, j))
      }
    }
    
    cat(sprintf('        self.wait(0.5)\n\n'))
  }
  
  cat('        self.wait(2)\n')
  
  sink()
  
  # Criar arquivo de dados JSON para Manim
  json_file <- file.path(output_dir, "blouse_data.json")
  
  # Simplificar passos para JSON (remover funções, etc.)
  json_steps <- lapply(steps, function(step) {
    list(
      title = step$title,
      n_points = length(step$points),
      n_lines = length(step$lines),
      n_curves = length(step$curves)
    )
  })
  
  jsonlite::write_json(json_steps, json_file, pretty = TRUE, auto_unbox = TRUE)
  
  message("Manim files created in: ", output_dir)
  message("  Python script: ", python_script)
  message("  Data file: ", json_file)
  message("\nTo render: manim -pql animate_blouse.py BlouseDrafting")
  
  invisible(python_script)
}

#' Preparar dados para visualização 3D (futuro)
#'
#' @param block Saída de draft_*()
#' @return Lista com dados preparados para 3D
prepare_for_3d <- function(block) {
  
  # Estrutura para futura integração com rgl/threejs
  list(
    vertices = extract_vertices_3d(block),
    edges = extract_edges_3d(block),
    faces = NULL,  # Será preenchido pela triangulação
    metadata = list(
      piece_type = "blouse",
      method = "sophia_jobim",
      scale = "cm"
    )
  )
}

#' Extrair vértices para visualização 3D
extract_vertices_3d <- function(block) {
  vertices <- list()
  idx <- 1
  
  # Frente (z = 0)
  for (pt in block$front$points) {
    vertices[[idx]] <- c(pt[1], pt[2], 0)
    idx <- idx + 1
  }
  
  # Costas (z = 0.5 para separação visual)
  for (pt in block$back$points) {
    vertices[[idx]] <- c(pt[1], pt[2], 0.5)
    idx <- idx + 1
  }
  
  do.call(rbind, vertices)
}

#' Extrair arestas para visualização 3D
extract_edges_3d <- function(block) {
  edges <- list()
  idx <- 1
  vertex_map <- list()
  v_idx <- 1
  
  # Mapear pontos para índices
  for (name in names(block$front$points)) {
    vertex_map[[paste0("front_", name)]] <- v_idx
    v_idx <- v_idx + 1
  }
  for (name in names(block$back$points)) {
    vertex_map[[paste0("back_", name)]] <- v_idx
    v_idx <- v_idx + 1
  }
  
  # Arestas da frente
  for (seam in block$front$seams) {
    from_id <- vertex_map[[paste0("front_", seam$from)]]
    to_id <- vertex_map[[paste0("front_", seam$to)]]
    
    if (!is.null(from_id) && !is.null(to_id)) {
      edges[[idx]] <- c(from_id, to_id)
      idx <- idx + 1
    }
  }
  
  # Arestas das costas
  for (seam in block$back$seams) {
    from_id <- vertex_map[[paste0("back_", seam$from)]]
    to_id <- vertex_map[[paste0("back_", seam$to)]]
    
    if (!is.null(from_id) && !is.null(to_id)) {
      edges[[idx]] <- c(from_id, to_id)
      idx <- idx + 1
    }
  }
  
  do.call(rbind, edges)
}

# Operador null-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b