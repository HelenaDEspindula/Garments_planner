#' =============================================================================
#' Pattern Plot — Geração de Gráficos a partir de .rds
#' =============================================================================
#' Lê bloco de arquivo .rds e gera gráficos.
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_notation.R"))
source(here("Rscripts", "pattern_io.R"))

#' Gerar gráfico a partir de arquivo .rds
#'
#' @param block_name Nome do bloco salvo (ex: "blusa_sj_size48")
#' @param output_filename Nome base para o arquivo de saída
#' @param title Título do gráfico
#' @param subtitle Subtítulo
#' @param show_points Mostrar labels dos pontos?
#' @param show_grid Mostrar grid de referência?
#' @param piece_type "front", "back" ou "both"
#' @param width Largura em polegadas
#' @param height Altura em polegadas
#' @param dpi Resolução do PNG
#' @return Caminho do arquivo gerado
plot_from_rds <- function(block_name,
                          output_filename = NULL,
                          title = NULL,
                          subtitle = NULL,
                          show_points = TRUE,
                          show_grid = TRUE,
                          piece_type = "both",
                          width = 7,
                          height = 7,
                          dpi = 150) {
  
  block <- load_block(block_name)
  
  if (is.null(output_filename)) {
    output_filename <- paste0(block_name, "_plot")
  }
  
  plot_pattern_notebook(
    block = block,
    filename = output_filename,
    title = title,
    subtitle = subtitle,
    show_points = show_points,
    show_grid = show_grid,
    piece_type = piece_type,
    width = width,
    height = height,
    dpi = dpi
  )
}

#' Gerar gráfico para o caderno (PNG cacheado)
plot_pattern_notebook <- function(block, 
                                  filename,
                                  title = NULL,
                                  subtitle = NULL,
                                  show_points = TRUE,
                                  show_grid = TRUE,
                                  piece_type = "both",
                                  width = 7,
                                  height = 7,
                                  dpi = 150) {
  
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
  
  if (is.null(title)) {
    title <- "Pattern Draft"
  }
  
  p <- create_pattern_plot(
    width = dim_width,
    height = dim_height,
    grid_spacing = if (show_grid) 5 else NULL,
    title = title,
    subtitle = subtitle
  )
  
  if (piece_type %in% c("front", "both")) {
    front <- if (piece_type == "both") block$front else block
    p <- add_piece_to_plot(p, front, "front", show_points)
  }
  
  if (piece_type %in% c("back", "both")) {
    back <- if (piece_type == "both") block$back else block
    p <- add_piece_to_plot(p, back, "back", show_points)
  }
  
  if (piece_type == "both" && !is.null(block$curves$curve_armscye)) {
    p <- p + annotate("path",
                      x = block$curves$curve_armscye$x,
                      y = block$curves$curve_armscye$y,
                      color = "#333333", linewidth = 1.5)
  }
  
  cache_file <- here("images", "cache", paste0(filename, ".png"))
  ggsave(cache_file, p, width = width, height = height, dpi = dpi,
         device = "png", create.dir = TRUE, bg = "white")
  
  message("Plot saved: ", cache_file)
  return(cache_file)
}

#' Adicionar peça ao plot
add_piece_to_plot <- function(p, piece, piece_type, show_points) {
  
  color <- if (piece_type == "front") {
    FREESEWING_COLORS$fabric_lining
  } else {
    FREESEWING_COLORS$fabric_interfacing
  }
  
  pts <- piece$points
  
  for (seam in piece$seams) {
    from_pt <- pts[[seam$from]]
    to_pt <- pts[[seam$to]]
    
    if (is.null(from_pt) || is.null(to_pt)) next
    
    lty <- if (grepl("dart", seam$name)) "dashed" else "solid"
    lwd <- if (grepl("dart", seam$name)) 0.8 else 1.2
    
    p <- p + annotate("segment",
                      x = from_pt[1], xend = to_pt[1],
                      y = from_pt[2], yend = to_pt[2],
                      color = color, linewidth = lwd, linetype = lty)
  }
  
  for (curve_data in piece$curves) {
    if (is.null(curve_data) || nrow(curve_data) < 2) next
    p <- p + annotate("path",
                      x = curve_data$x, y = curve_data$y,
                      color = color, linewidth = 1.2)
  }
  
  if (show_points) {
    for (pt_name in names(pts)) {
      pt <- pts[[pt_name]]
      if (is.null(pt)) next
      label <- gsub("point_", "", pt_name)
      p <- p + 
        annotate("point", x = pt[1], y = pt[2], size = 2, color = "#333333") +
        annotate("text", x = pt[1], y = pt[2], label = label,
                 hjust = -0.3, vjust = -0.3, size = 2.5, 
                 color = "#333333", fontface = "bold")
    }
  }
  
  all_x <- sapply(pts, `[`, 1)
  all_y <- sapply(pts, `[`, 2)
  mid_x <- mean(all_x, na.rm = TRUE)
  mid_y <- mean(all_y, na.rm = TRUE)
  
  p <- p + annotate("text", x = mid_x, y = mid_y,
                    label = toupper(piece_type), size = 8, color = color,
                    fontface = "bold", angle = 90, alpha = 0.3)
  
  return(p)
}

#' Criar plot base com grid
create_pattern_plot <- function(width, height, grid_spacing = 5,
                                title = NULL, subtitle = NULL) {
  x_margin <- width * 0.1
  y_margin <- height * 0.1
  
  p <- ggplot() +
    coord_fixed(xlim = c(-x_margin, width + x_margin),
                ylim = c(-y_margin, height + y_margin)) +
    labs(title = title, subtitle = subtitle,
         x = "Width (cm)", y = "Height (cm)") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#F5F5F5", linewidth = 0.1),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  if (!is.null(grid_spacing)) {
    p <- p +
      geom_hline(yintercept = seq(0, height, by = grid_spacing), 
                 color = "#E0E0E0", linewidth = 0.3) +
      geom_vline(xintercept = seq(0, width, by = grid_spacing), 
                 color = "#E0E0E0", linewidth = 0.3)
  }
  
  p
}

#' Salvar imagem do padrão em estrutura de pastas organizada por ano/mês
#' 
#' @param plot_file Caminho do arquivo gerado (do plot_pattern_notebook)
#' @param base_name Nome base descritivo (ex: "blouse_sophia_j")
#' @param width Largura para o markdown (ex: "80%")
#' @param height Altura para o markdown (ex: "0.85\\textheight")
#' @return String formatada para inclusão no markdown (impressa via cat)
save_pattern_image <- function(plot_file, base_name, 
                               width = "80%", 
                               height = "0.85\\textheight") {
  
  agora <- Sys.time()
  timestamp <- format(agora, "%Y-%m-%d_%Hh%M")
  ano <- format(agora, "%Y")
  mes_num <- as.numeric(format(agora, "%m"))
  
  mes_nomes <- c("01-jan", "02-fev", "03-mar", "04-abr", "05-mai", "06-jun",
                 "07-jul", "08-ago", "09-set", "10-out", "11-nov", "12-dez")
  mes_pasta <- mes_nomes[mes_num]
  
  output_filename <- paste0(timestamp, "--", base_name, ".png")
  
  # Caminho absoluto da pasta de destino
  pasta_destino <- file.path(getwd(), "images", ano, mes_pasta)
  dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)
  
  # Mover arquivo do cache para a pasta definitiva
  final_path <- file.path(pasta_destino, output_filename)
  file.copy(plot_file, final_path, overwrite = TRUE)
  unlink(plot_file)
  
  # Gerar tag markdown e imprimir
  img_md <- sprintf("![%s](%s){fig-pos='H' width=%s height=%s}\n\n",
                    base_name, final_path, width, height)
  
  cat(img_md)
  message("Image saved: ", final_path)
  invisible(final_path)
}

#' Salvar múltiplas imagens e gerar bloco markdown
#' 
#' @param plot_files Vetor de caminhos de arquivos
#' @param base_names Vetor de nomes descritivos
#' @param width Largura
#' @param height Altura
#' @return String formatada com todas as imagens
save_pattern_images <- function(plot_files, base_names, 
                                width = "80%", 
                                height = "0.85\\textheight") {
  
  purrr::walk2(plot_files, base_names, function(img, name) {
    save_pattern_image(img, name, width, height)
  })
}
