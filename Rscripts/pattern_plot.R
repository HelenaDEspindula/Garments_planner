#' =============================================================================
#' Pattern Plot — Geração de Gráficos
#' =============================================================================
#' Dois modos principais:
#'   1. plot_pattern_notebook(): Gráfico para o caderno (PDF cacheado)
#'   2. plot_pattern_fullscale(): PDF em tamanho real para impressão
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_notation.R"))

#' Gerar gráfico para o caderno (PDF cacheado em images/cache/)
#'
#' @param block Saída de draft_*() ou blusa completa
#' @param filename Nome base para o arquivo de cache
#' @param title Título do gráfico
#' @param subtitle Subtítulo
#' @param show_points Mostrar labels dos pontos?
#' @param show_grid Mostrar grid de referência?
#' @param show_construction Mostrar linhas de construção?
#' @param piece_type "front", "back" ou "both"
#' @param width Largura em polegadas
#' @param height Altura em polegadas
#' @return Caminho do arquivo gerado (invisível)
plot_pattern_notebook <- function(block, 
                                  filename,
                                  title = NULL,
                                  subtitle = NULL,
                                  show_points = TRUE,
                                  show_grid = TRUE,
                                  show_construction = TRUE,
                                  piece_type = "both",
                                  width = 10,
                                  height = 10) {
  
  # Determinar dimensões do plot
  if (piece_type == "both") {
    dim_width <- block$dimensions$dim_width
    dim_height <- block$dimensions$dim_height
  } else if (piece_type == "front") {
    dim_width <- block$front$metadata$dimensions$width
    dim_height <- block$front$metadata$dimensions$height
  } else {
    dim_width <- block$back$metadata$dimensions$width
    dim_height <- block$back$metadata$dimensions$height
  }
  
  # Título padrão
  if (is.null(title)) {
    title <- "Pattern Draft"
  }
  
  # Criar plot base
  p <- create_pattern_plot(
    width = dim_width,
    height = dim_height,
    grid_spacing = if (show_grid) 5 else NULL,
    title = title,
    subtitle = subtitle
  )
  
  # Adicionar peças conforme piece_type
  if (piece_type %in% c("front", "both")) {
    front <- if (piece_type == "both") block$front else block
    p <- add_piece_to_plot(p, front, "front", show_points, show_construction)
  }
  
  if (piece_type %in% c("back", "both")) {
    back <- if (piece_type == "both") block$back else block
    p <- add_piece_to_plot(p, back, "back", show_points, show_construction)
  }
  
  # Adicionar cava completa se for both
  if (piece_type == "both" && !is.null(block$curves$curve_armscye)) {
    p <- p + 
      geom_path(data = block$curves$curve_armscye, aes(x, y),
                color = "#333333", linewidth = 1.5)
  }
  
  # Salvar em cache
  cache_file <- here("images", "cache", paste0(filename, ".pdf"))
  
  ggsave(cache_file, p, 
         width = width, height = height,
         device = "pdf",
         create.dir = TRUE)
  
  message("Plot saved: ", cache_file)
  invisible(cache_file)
}

#' Adicionar uma peça ao plot
add_piece_to_plot <- function(p, piece, piece_type, show_points, show_construction) {
  
  color <- if (piece_type == "front") {
    FREESEWING_COLORS$fabric_lining
  } else {
    FREESEWING_COLORS$fabric_interfacing
  }
  
  pts <- piece$points
  
  # Desenhar costuras
  for (seam in piece$seams) {
    from_pt <- pts[[seam$from]]
    to_pt <- pts[[seam$to]]
    
    if (!is.null(from_pt) && !is.null(to_pt)) {
      # Verificar se é uma linha de pence (desenhar tracejado)
      lty <- if (grepl("dart", seam$name)) "dashed" else "solid"
      lwd <- if (grepl("dart", seam$name)) 0.8 else 1.2
      
      p <- p + geom_segment(
        aes(x = from_pt[1], xend = to_pt[1],
            y = from_pt[2], yend = to_pt[2]),
        color = color, linewidth = lwd, linetype = lty
      )
    }
  }
  
  # Desenhar curvas
  for (curve_name in names(piece$curves)) {
    curve_data <- piece$curves[[curve_name]]
    p <- p + geom_path(data = curve_data, aes(x, y),
                       color = color, linewidth = 1.2)
  }
  
  # Mostrar pontos
  if (show_points) {
    # Apenas pontos principais (não pontos de construção internos)
    main_points <- names(pts)[!grepl("dart_leg|dart_tip|_shifted|point_V|point_X", names(pts))]
    
    for (pt_name in main_points) {
      pt <- pts[[pt_name]]
      label <- gsub("point_", "", pt_name)
      
      p <- p + 
        annotate("point", x = pt[1], y = pt[2], 
                 size = 2.5, color = "#333333") +
        annotate("text", x = pt[1], y = pt[2], 
                 label = label, hjust = -0.3, vjust = -0.3,
                 size = 3, color = "#333333", fontface = "bold")
    }
  }
  
  # Label da peça
  if (piece_type == "front") {
    mid_x <- (min(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 1)) + 
                max(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 1))) / 2
    mid_y <- (min(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 2)) + 
                max(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 2))) / 2
    
    p <- p + annotate("text", x = mid_x, y = mid_y,
                      label = "FRENTE", size = 8, color = color,
                      fontface = "bold", angle = 90, alpha = 0.3)
  } else {
    mid_x <- (min(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 1)) + 
                max(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 1))) / 2
    mid_y <- (min(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 2)) + 
                max(sapply(pts[grepl("point_[A-Z]", names(pts))], `[`, 2))) / 2
    
    p <- p + annotate("text", x = mid_x, y = mid_y,
                      label = "COSTAS", size = 8, color = color,
                      fontface = "bold", angle = 90, alpha = 0.3)
  }
  
  p
}

#' Gerar PDF em tamanho real para plotter ou impressão
#'
#' @param block Saída de draft_*()
#' @param output_file Caminho do arquivo de saída
#' @param paper_size "A0", "A1", "A2", "A3", "A4"
#' @param include_seam_allowance Incluir margem de costura?
#' @param seam_allowance Margem em cm
#' @return Caminho do arquivo gerado
plot_pattern_fullscale <- function(block, 
                                   output_file,
                                   paper_size = "A0",
                                   include_seam_allowance = FALSE,
                                   seam_allowance = 1.5) {
  
  # Dimensões de papel
  paper_dims <- list(
    A4 = c(21.0, 29.7),
    A3 = c(29.7, 42.0),
    A2 = c(42.0, 59.4),
    A1 = c(59.4, 84.1),
    A0 = c(84.1, 118.9)
  )
  
  dims <- paper_dims[[paper_size]]
  
  # Margem de segurança
  margin <- 2
  
  # Criar plot base em escala real
  p <- ggplot() +
    coord_fixed(
      xlim = c(-margin, dims[1] + margin),
      ylim = c(-margin, dims[2] + margin)
    ) +
    theme_void()
  
  # Adicionar peças (frente e costas)
  for (piece_name in c("front", "back")) {
    piece <- block[[piece_name]]
    color <- if (piece_name == "front") "#000000" else "#000000"
    
    # Costuras
    for (seam in piece$seams) {
      from_pt <- piece$points[[seam$from]]
      to_pt <- piece$points[[seam$to]]
      
      if (!is.null(from_pt) && !is.null(to_pt)) {
        p <- p + geom_segment(
          aes(x = from_pt[1], xend = to_pt[1],
              y = from_pt[2], yend = to_pt[2]),
          color = color, linewidth = 1.5
        )
      }
    }
    
    # Curvas
    for (curve_data in piece$curves) {
      p <- p + geom_path(data = curve_data, aes(x, y),
                         color = color, linewidth = 1.5)
    }
    
    # Margem de costura (se solicitada)
    if (include_seam_allowance) {
      # Simplificação: offset visual
      p <- add_seam_allowance_lines(p, piece, seam_allowance)
    }
  }
  
  # Adicionar escala e informações
  p <- p +
    annotate("text", x = margin, y = margin,
             label = paste("Escala 1:1 |", paper_size),
             hjust = 0, vjust = 0, size = 8, color = "grey50") +
    annotate("rect", xmin = margin, xmax = margin + 5,
             ymin = dims[2] - margin - 1, ymax = dims[2] - margin + 1,
             fill = NA, color = "black", linewidth = 1) +
    annotate("text", x = margin + 2.5, y = dims[2] - margin - 2,
             label = "5 cm", size = 6)
  
  # Salvar
  ggsave(output_file, p,
         width = dims[1], height = dims[2],
         units = "cm", limitsize = FALSE,
         device = "pdf")
  
  message("Full-scale pattern saved: ", output_file)
  invisible(output_file)
}

#' Adicionar linhas de margem de costura (versão simplificada)
add_seam_allowance_lines <- function(p, piece, allowance) {
  # Para cada costura, adicionar linha paralela
  for (seam in piece$seams) {
    from_pt <- piece$points[[seam$from]]
    to_pt <- piece$points[[seam$to]]
    
    if (!is.null(from_pt) && !is.null(to_pt)) {
      # Calcular offset perpendicular
      angle <- atan2(to_pt[2] - from_pt[2], to_pt[1] - from_pt[1])
      perp_angle <- angle + pi/2
      
      from_offset <- c(
        from_pt[1] + allowance * cos(perp_angle),
        from_pt[2] + allowance * sin(perp_angle)
      )
      to_offset <- c(
        to_pt[1] + allowance * cos(perp_angle),
        to_pt[2] + allowance * sin(perp_angle)
      )
      
      p <- p + geom_segment(
        aes(x = from_offset[1], xend = to_offset[1],
            y = from_offset[2], yend = to_offset[2]),
        color = "grey50", linewidth = 0.5, linetype = "dashed"
      )
    }
  }
  
  p
}

#' Gerar tiles A4 para impressão caseira
#'
#' @param block Saída de draft_*()
#' @param output_file Caminho do PDF de saída
#' @param paper_size "A4" ou "Letter"
#' @param overlap Sobreposição entre tiles (cm)
#' @return Caminho do arquivo gerado
plot_pattern_tiled <- function(block, output_file,
                               paper_size = "A4", overlap = 1) {
  
  paper_dims <- list(
    A4 = c(21.0, 29.7),
    Letter = c(21.6, 27.9)
  )
  
  dims <- paper_dims[[paper_size]]
  margin <- 1.5
  tile_width <- dims[1] - 2 * margin
  tile_height <- dims[2] - 2 * margin
  
  # Coletar todos os pontos para determinar bounds
  all_x <- numeric(0)
  all_y <- numeric(0)
  
  for (piece_name in c("front", "back")) {
    piece <- block[[piece_name]]
    for (pt in piece$points) {
      all_x <- c(all_x, pt[1])
      all_y <- c(all_y, pt[2])
    }
  }
  
  x_min <- min(all_x) - 2
  x_max <- max(all_x) + 2
  y_min <- min(all_y) - 2
  y_max <- max(all_y) + 2
  
  n_cols <- ceiling((x_max - x_min) / (tile_width - overlap))
  n_rows <- ceiling((y_max - y_min) / (tile_height - overlap))
  
  # Criar PDF multi-página
  pdf(output_file, width = dims[1]/2.54, height = dims[2]/2.54)
  
  for (row in 1:n_rows) {
    for (col in 1:n_cols) {
      x_start <- x_min + (col - 1) * (tile_width - overlap)
      x_end <- x_start + tile_width
      y_start <- y_min + (row - 1) * (tile_height - overlap)
      y_end <- y_start + tile_height
      
      p <- create_tile_plot(block, x_start, x_end, y_start, y_end,
                            row, col, n_rows, n_cols)
      print(p)
    }
  }
  
  dev.off()
  message("Tiled pattern saved: ", output_file)
  invisible(output_file)
}

#' Criar plot de um tile individual
create_tile_plot <- function(block, x_start, x_end, y_start, y_end,
                             row, col, n_rows, n_cols) {
  
  p <- ggplot() +
    coord_fixed(xlim = c(x_start, x_end), ylim = c(y_start, y_end)) +
    theme_void()
  
  # Adicionar peças
  for (piece_name in c("front", "back")) {
    piece <- block[[piece_name]]
    
    for (seam in piece$seams) {
      from_pt <- piece$points[[seam$from]]
      to_pt <- piece$points[[seam$to]]
      
      if (!is.null(from_pt) && !is.null(to_pt)) {
        p <- p + geom_segment(
          aes(x = from_pt[1], xend = to_pt[1],
              y = from_pt[2], yend = to_pt[2]),
          color = "black", linewidth = 1.5
        )
      }
    }
  }
  
  # Marcas de registro nos cantos
  p <- p +
    annotate("point", x = x_start, y = y_start, shape = 3, size = 5) +
    annotate("point", x = x_end, y = y_start, shape = 3, size = 5) +
    annotate("point", x = x_start, y = y_end, shape = 3, size = 5) +
    annotate("point", x = x_end, y = y_end, shape = 3, size = 5) +
    annotate("text", x = (x_start + x_end)/2, y = y_end - 1,
             label = paste("Tile", (row-1)*n_cols + col, "de", n_rows*n_cols),
             size = 6, color = "grey50")
  
  p
}

#' Criar plot base com grid
create_pattern_plot <- function(width, height, grid_spacing = 5,
                                title = NULL, subtitle = NULL) {
  
  x_margin <- width * 0.1
  y_margin <- height * 0.1
  
  p <- ggplot() +
    coord_fixed(
      xlim = c(-x_margin, width + x_margin),
      ylim = c(-y_margin, height + y_margin)
    ) +
    labs(title = title, subtitle = subtitle,
         x = "Width (cm)", y = "Height (cm)") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#F5F5F5", linewidth = 0.1),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  # Adicionar grid personalizado se solicitado
  if (!is.null(grid_spacing)) {
    p <- p +
      geom_hline(yintercept = seq(0, height, by = grid_spacing),
                 color = "#E0E0E0", linewidth = 0.3) +
      geom_vline(xintercept = seq(0, width, by = grid_spacing),
                 color = "#E0E0E0", linewidth = 0.3)
  }
  
  p
}