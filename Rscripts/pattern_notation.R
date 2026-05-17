#' FreeSewing-compatible notation system
#' 
#' Based on: https://freesewing.eu/docs/about/notation/
#' All measurements in metric system (centimeters)

# Color palette
FREESEWING_COLORS <- list(
  fabric_primary = "#212121",
  fabric_lining = "#1976D2",
  fabric_interfacing = "#F57C00",
  seam_allowance = "#757575",
  grainline = "#388E3C",
  cut_on_fold = "#D32F2F",
  dimensions = "#1565C0",
  notch_default = "#212121",
  notch_back = "#D32F2F",
  button = "#212121",
  buttonhole = "#212121",
  bartack = "#F57C00",
  snap = "#757575",
  note_line = "#455A64",
  mark_line = "#FF5722",
  contrast_line = "#00BCD4",
  help_line = "#9E9E9E"
)

# Line types
FREESEWING_LINETYPES <- list(
  seam = "solid",
  seam_allowance = "dashed",
  grainline = "dotted",
  cut_on_fold = "twodash",
  dimension = "dotted",
  note = "dashed",
  mark = "solid",
  contrast = "solid",
  help = "dotted"
)

# Line widths (in mm for FreeSewing compatibility)
FREESEWING_LINEWIDTHS <- list(
  default = 1.0,
  thin = 0.5,
  thick = 2.0,
  seam_allowance = 0.8,
  dimension = 0.5
)

#' Add FreeSewing-style notch to pattern
#' 
#' ⊙ Front notch (default): dot inside circle
#' ⊗ Back notch: cross inside circle
#' 
#' @param plot ggplot object
#' @param x,y Coordinates in cm
#' @param type "front" or "back"
#' @param size Notch size in cm
#' @return ggplot object with notch added
add_notch <- function(plot, x, y, type = "front", size = 0.3) {
  if (type == "front") {
    plot +
      annotate("point", x = x, y = y, 
               size = size * 3, color = FREESEWING_COLORS$notch_default) +
      annotate("point", x = x, y = y, 
               size = size * 6, shape = 1, 
               color = FREESEWING_COLORS$notch_default)
  } else {
    plot +
      annotate("point", x = x, y = y, 
               size = size * 6, shape = 1,
               color = FREESEWING_COLORS$notch_back) +
      annotate("text", x = x, y = y, 
               label = "+", size = size * 3,
               color = FREESEWING_COLORS$notch_back)
  }
}

#' Add grainline indicator
#' @param plot ggplot object
#' @param x1,y1,x2,y2 Line coordinates in cm
#' @param arrow_size Arrow head size in cm
#' @return ggplot object
add_grainline <- function(plot, x1, y1, x2, y2, arrow_size = 0.5) {
  mid_x <- (x1 + x2) / 2
  mid_y <- (y1 + y2) / 2
  angle <- atan2(y2 - y1, x2 - x1) * 180 / pi
  
  plot +
    geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2),
                 color = FREESEWING_COLORS$grainline,
                 linetype = FREESEWING_LINETYPES$grainline,
                 linewidth = 0.5) +
    annotate("text", x = mid_x, y = mid_y,
             label = "→",
             size = arrow_size * 3,
             angle = angle,
             color = FREESEWING_COLORS$grainline)
}

#' Add cut-on-fold indicator
#' @param plot ggplot object
#' @param x1,y1,x2,y2 Fold line coordinates
#' @return ggplot object
add_cut_on_fold <- function(plot, x1, y1, x2, y2) {
  angle <- atan2(y2 - y1, x2 - x1) * 180 / pi
  mid <- midpoint(c(x1, y1), c(x2, y2))
  
  plot +
    geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2),
                 color = FREESEWING_COLORS$cut_on_fold,
                 linetype = FREESEWING_LINETYPES$cut_on_fold,
                 linewidth = 1.0) +
    annotate("text", x = mid[1], y = mid[2] + 0.5,
             label = "⬇", size = 5,
             angle = angle,
             color = FREESEWING_COLORS$cut_on_fold)
}

#' Add button marking
#' @param plot ggplot object
#' @param x,y Button position
#' @param size Button size in cm
#' @return ggplot object
add_button <- function(plot, x, y, size = 0.5) {
  plot +
    annotate("point", x = x, y = y,
             size = size * 5, shape = 21,
             fill = "white", color = FREESEWING_COLORS$button) +
    annotate("point", x = x, y = y,
             size = size * 2, shape = 3,
             color = FREESEWING_COLORS$button)
}

#' Add buttonhole marking
#' @param plot ggplot object
#' @param x,y Buttonhole start position
#' @param length Buttonhole length in cm
#' @param angle Angle in degrees
#' @return ggplot object
add_buttonhole <- function(plot, x, y, length = 1.5, angle = 0) {
  angle_rad <- angle * pi / 180
  xend <- x + length * cos(angle_rad)
  yend <- y + length * sin(angle_rad)
  
  plot +
    annotate("segment", x = x, xend = xend, y = y, yend = yend,
             color = FREESEWING_COLORS$buttonhole, linewidth = 1.5) +
    annotate("point", x = xend + 0.2 * cos(angle_rad + pi/2),
             y = yend + 0.2 * sin(angle_rad + pi/2),
             size = 2, shape = 1, color = FREESEWING_COLORS$buttonhole)
}

#' Add scale box for print verification
#' @param plot ggplot object
#' @param x,y Bottom-left corner position
#' @param size Box size in cm (default 5cm)
#' @return ggplot object
add_scale_box <- function(plot, x, y, size = 5) {
  plot +
    annotate("rect", 
             xmin = x, xmax = x + size,
             ymin = y, ymax = y + size,
             fill = NA, color = "black", linewidth = 0.5) +
    annotate("text", x = x + size/2, y = y + size/2,
             label = paste(size, "cm"),
             size = 3, color = "black") +
    annotate("text", x = x + size/2, y = y - 0.3,
             label = "Verify print scale",
             size = 2.5, color = "grey50")
}

#' Add title block to pattern piece
#' @param plot ggplot object
#' @param piece_name Pattern piece name
#' @param piece_number Piece number
#' @param size_name Size identifier
#' @param x,y Position for title block
#' @return ggplot object
add_title_block <- function(plot, piece_name, piece_number, 
                            size_name, x, y) {
  title_text <- paste0(
    piece_number, " - ", piece_name, "\n",
    "Size: ", size_name, "\n",
    format(Sys.Date(), "%Y-%m-%d")
  )
  
  plot +
    annotate("text", x = x, y = y,
             label = title_text,
             hjust = 0, vjust = 1,
             size = 3, fontface = "bold",
             color = FREESEWING_COLORS$fabric_primary)
}

#' Add seam allowance line
#' @param plot ggplot object
#' @param seam_line_points Data frame of seam points
#' @param allowance Seam allowance in cm
#' @return ggplot object
add_seam_allowance <- function(plot, seam_line_points, allowance = 1.5) {
  # Create offset line (simplified - real implementation would need proper offset algorithm)
  offset_points <- seam_line_points
  offset_points$x <- seam_line_points$x + allowance
  offset_points$y <- seam_line_points$y + allowance
  
  n <- nrow(offset_points)
  for (i in 1:(n-1)) {
    plot <- plot +
      geom_segment(
        data = data.frame(
          x = offset_points$x[i], xend = offset_points$x[i+1],
          y = offset_points$y[i], yend = offset_points$y[i+1]
        ),
        aes(x = x, y = y, xend = xend, yend = yend),
        color = FREESEWING_COLORS$seam_allowance,
        linetype = FREESEWING_LINETYPES$seam_allowance,
        linewidth = FREESEWING_LINEWIDTHS$seam_allowance
      )
  }
  
  plot
}