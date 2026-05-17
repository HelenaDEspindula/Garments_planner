#' Pattern printing functions for full-scale output
#'
#' Creates PDFs suitable for printing on home printers or large format plotters

#' Create tiled pattern for home printing
#' 
#' Splits large pattern into A4/Letter tiles with registration marks
#' 
#' @param points_df Pattern points data frame
#' @param output_file Output PDF path
#' @param paper "A4" or "Letter"
#' @param overlap Overlap between tiles in cm
#' @param margin Page margin in cm
#' @return Path to output file
create_tiled_pattern <- function(points_df, output_file,
                                 paper = "A4", overlap = 1,
                                 margin = 1.5) {
  # Paper dimensions
  paper_dims <- list(
    A4 = c(21.0, 29.7),
    Letter = c(21.6, 27.9)
  )
  
  dims <- paper_dims[[paper]]
  tile_width <- dims[1] - 2 * margin
  tile_height <- dims[2] - 2 * margin
  
  # Calculate pattern bounds
  x_min <- min(points_df$x)
  x_max <- max(points_df$x)
  y_min <- min(points_df$y)
  y_max <- max(points_df$y)
  
  # Calculate number of tiles needed
  n_cols <- ceiling((x_max - x_min) / (tile_width - overlap))
  n_rows <- ceiling((y_max - y_min) / (tile_height - overlap))
  
  # Create multi-page PDF
  pdf(output_file, width = dims[1]/2.54, height = dims[2]/2.54)  # Convert to inches
  
  for (row in 1:n_rows) {
    for (col in 1:n_cols) {
      # Calculate tile bounds
      x_start <- x_min + (col - 1) * (tile_width - overlap)
      x_end <- x_start + tile_width
      y_start <- y_min + (row - 1) * (tile_height - overlap)
      y_end <- y_start + tile_height
      
      # Create tile plot
      p <- create_pattern_plot(
        width = tile_width,
        height = tile_height,
        grid_spacing = 5
      )
      
      # Add pattern lines within this tile
      p <- add_pattern_piece(p, points_df, label_points = FALSE)
      
      # Add registration marks at corners
      p <- p +
        annotate("point", x = x_start, y = y_start, 
                 shape = 3, size = 5, color = "black") +
        annotate("point", x = x_end, y = y_start, 
                 shape = 3, size = 5, color = "black") +
        annotate("point", x = x_start, y = y_end, 
                 shape = 3, size = 5, color = "black") +
        annotate("point", x = x_end, y = y_end, 
                 shape = 3, size = 5, color = "black")
      
      # Add tile identifier
      p <- p +
        annotate("text", x = x_start + tile_width/2, 
                 y = y_start + tile_height - 1,
                 label = paste("Tile", (row-1)*n_cols + col, "of", n_rows*n_cols,
                               "\nRow", row, "Col", col),
                 size = 3, color = "grey50")
      
      # Set view to tile
      p <- p + coord_fixed(
        xlim = c(x_start - margin, x_end + margin),
        ylim = c(y_start - margin, y_end + margin)
      )
      
      print(p)
    }
  }
  
  dev.off()
  message("Tiled pattern saved: ", output_file)
  output_file
}

#' Create A0 plotter-ready pattern
#' @param points_df Pattern points data frame
#' @param output_file Output PDF path
#' @param include_seam_allowance Include seam allowance lines
#' @param seam_allowance Seam allowance in cm
#' @return Path to output file
create_plotter_pattern <- function(points_df, output_file,
                                   include_seam_allowance = TRUE,
                                   seam_allowance = 1.5) {
  # Calculate pattern bounds with margin
  x_min <- min(points_df$x) - 5
  x_max <- max(points_df$x) + 5
  y_min <- min(points_df$y) - 5
  y_max <- max(points_df$y) + 5
  
  width <- x_max - x_min
  height <- y_max - y_min
  
  # Create base plot
  p <- create_pattern_plot(
    width = width,
    height = height,
    grid_spacing = 10,
    title = "Full-Scale Pattern - Plotter Ready"
  )
  
  # Add pattern
  p <- add_pattern_piece(p, points_df, label_points = FALSE)
  
  # Add seam allowance if requested
  if (include_seam_allowance) {
    p <- add_seam_allowance(p, points_df, seam_allowance)
  }
  
  # Add scale box
  p <- add_scale_box(p, x_min + 2, y_min + 2)
  
  # Save at full scale (1cm = 1cm)
  ggsave(output_file, p,
         width = width, height = height,
         units = "cm", limitsize = FALSE,
         device = "pdf")
  
  message("Plotter-ready pattern saved: ", output_file)
  output_file
}