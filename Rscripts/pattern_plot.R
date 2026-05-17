#' Pattern plotting functions with caching
#' 
#' All plots are saved as PDF images in images/cache/ for faster recompilation.

#' Generate cache filename for a plot
#' @param name Plot identifier
#' @param params List of parameters affecting the plot
#' @return Cache file path
cache_filename <- function(name, params = NULL) {
  if (!is.null(params)) {
    param_hash <- digest::digest(params, algo = "md5")
    filename <- paste0(name, "_", param_hash, ".pdf")
  } else {
    filename <- paste0(name, ".pdf")
  }
  here("images", "cache", filename)
}

#' Save or load cached plot
#' @param name Plot identifier
#' @param plot_func Function that generates the plot
#' @param params Parameters for cache invalidation
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @return ggplot object
cached_plot <- function(name, plot_func, params = NULL, 
                        width = 8, height = 10) {
  cache_file <- cache_filename(name, params)
  
  if (file.exists(cache_file)) {
    # Return cached version
    return(knitr::include_graphics(cache_file))
  }
  
  # Generate and save plot
  plot <- plot_func()
  
  ggsave(cache_file, plot, 
         width = width, height = height, 
         device = "pdf",
         limitsize = FALSE)
  
  # Return the plot
  plot
}

#' Create base pattern plot with grid
#' @param width Plot width in cm
#' @param height Plot height in cm
#' @param grid_spacing Grid spacing in cm (default 5cm)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @return ggplot object
create_pattern_plot <- function(width, height, 
                                grid_spacing = 5,
                                title = NULL, 
                                subtitle = NULL) {
  # Add 10% margin
  x_margin <- width * 0.1
  y_margin <- height * 0.1
  
  ggplot() +
    # Major grid (every 5cm)
    geom_hline(yintercept = seq(0, height + y_margin, by = grid_spacing),
               color = "#E0E0E0", linewidth = 0.3) +
    geom_vline(xintercept = seq(-x_margin, width + x_margin, by = grid_spacing),
               color = "#E0E0E0", linewidth = 0.3) +
    # Minor grid (every 1cm)
    geom_hline(yintercept = seq(0, height + y_margin, by = 1),
               color = "#F5F5F5", linewidth = 0.1) +
    geom_vline(xintercept = seq(-x_margin, width + x_margin, by = 1),
               color = "#F5F5F5", linewidth = 0.1) +
    # Coordinate system
    coord_fixed(
      xlim = c(-x_margin, width + x_margin),
      ylim = c(-y_margin, height + y_margin)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Width (cm)",
      y = "Height (cm)"
    ) +
    theme_pattern()
}

#' Add pattern piece to plot
#' @param plot Existing ggplot object
#' @param points_df Data frame with x, y, name columns
#' @param color Line color
#' @param linewidth Line width
#' @param linetype Line type
#' @param label_points Logical, whether to label points
#' @param label_size Text size for point labels
#' @return ggplot object with added layers
add_pattern_piece <- function(plot, points_df, 
                              color = FREESEWING_COLORS$fabric_primary,
                              linewidth = 1.0,
                              linetype = "solid",
                              label_points = TRUE,
                              label_size = 3) {
  # Add lines connecting points in order
  n <- nrow(points_df)
  if (n > 1) {
    for (i in 1:(n-1)) {
      plot <- plot +
        geom_segment(
          data = data.frame(
            x = points_df$x[i], xend = points_df$x[i+1],
            y = points_df$y[i], yend = points_df$y[i+1]
          ),
          aes(x = x, y = y, xend = xend, yend = yend),
          color = color, linewidth = linewidth, linetype = linetype
        )
    }
  }
  
  # Add points
  plot <- plot +
    geom_point(data = points_df, 
               aes(x = x, y = y),
               size = 2, color = color)
  
  # Add labels if requested
  if (label_points) {
    plot <- plot +
      geom_text(data = points_df,
                aes(x = x, y = y, label = name),
                hjust = -0.5, vjust = -0.5,
                size = label_size, color = color)
  }
  
  plot
}

#' Add measurement annotation to pattern plot
#' @param plot Existing ggplot object
#' @param p1,p2 Points between which to show measurement
#' @param label Measurement label
#' @param offset Distance from line for text placement
#' @param color Text color
#' @param size Text size
#' @return ggplot object
add_measurement <- function(plot, p1, p2, label, 
                            offset = 0.5, color = "#1565C0", 
                            size = 3) {
  mid <- midpoint(p1, p2)
  angle <- calc_angle(p1, p2)
  perp_angle <- angle + pi/2
  
  label_pos <- c(mid[1] + offset * cos(perp_angle),
                 mid[2] + offset * sin(perp_angle))
  
  text_angle <- angle * 180 / pi
  if (text_angle > 90 || text_angle < -90) {
    text_angle <- text_angle - 180
  }
  
  plot +
    annotate("segment",
             x = p1[1], xend = p2[1],
             y = p1[2], yend = p2[2],
             color = color, linewidth = 0.3, linetype = "dotted") +
    annotate("text",
             x = label_pos[1], y = label_pos[2],
             label = label,
             angle = text_angle,
             color = color, size = size,
             fontface = "italic")
}

#' Create full-scale printable pattern plot
#' @param points_df Data frame with pattern points
#' @param filename Output filename
#' @param paper_size Paper size ("A4", "A3", "A0")
#' @return Path to saved file
create_printable_pattern <- function(points_df, filename, 
                                     paper_size = "A0") {
  # Paper dimensions in cm
  paper_dims <- list(
    A4 = c(21.0, 29.7),
    A3 = c(29.7, 42.0),
    A2 = c(42.0, 59.4),
    A1 = c(59.4, 84.1),
    A0 = c(84.1, 118.9)
  )
  
  dims <- paper_dims[[paper_size]]
  
  # Create full-scale plot
  plot <- create_pattern_plot(
    width = dims[1], 
    height = dims[2],
    grid_spacing = 10,
    title = paste("Full-Scale Pattern -", paper_size)
  )
  
  plot <- add_pattern_piece(plot, points_df, label_points = FALSE)
  
  # Save at actual size (1cm = 1cm)
  ggsave(filename, plot,
         width = dims[1], height = dims[2],
         units = "cm", limitsize = FALSE,
         device = "pdf")
  
  filename
}