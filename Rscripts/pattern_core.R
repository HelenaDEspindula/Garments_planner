#' Core geometric functions for pattern drafting
#' 
#' These functions handle the mathematical operations needed
#' to translate drafting instructions into coordinates.
#' All measurements are in CENTIMETERS.

#' Calculate Euclidean distance between two points
#' @param p1 Vector c(x, y)
#' @param p2 Vector c(x, y)
#' @return Distance in cm
calc_distance <- function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}

#' Calculate angle between two points
#' @param p1 Start point c(x, y)
#' @param p2 End point c(x, y)
#' @return Angle in radians from positive x-axis
calc_angle <- function(p1, p2) {
  atan2(p2[2] - p1[2], p2[1] - p1[1])
}

#' Calculate midpoint between two points
#' @param p1 Vector c(x, y)
#' @param p2 Vector c(x, y)
#' @return Midpoint coordinates c(x, y)
midpoint <- function(p1, p2) {
  c((p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2)
}

#' Extend a line from p1 through p2 by given distance
#' @param p1 Start point c(x, y)
#' @param p2 Direction point c(x, y)
#' @param distance Distance in cm to extend beyond p2
#' @return Extended point c(x, y)
extend_line <- function(p1, p2, distance) {
  angle <- calc_angle(p1, p2)
  c(p2[1] + distance * cos(angle),
    p2[2] + distance * sin(angle))
}

#' Calculate perpendicular offset point from a line segment
#' @param p1 Start point c(x, y)
#' @param p2 End point c(x, y)
#' @param offset Distance in cm (positive = left, negative = right)
#' @param position Fraction along line (0 = p1, 1 = p2)
#' @return Offset point c(x, y)
perpendicular_offset <- function(p1, p2, offset, position = 0.5) {
  point <- c(p1[1] + position * (p2[1] - p1[1]),
             p1[2] + position * (p2[2] - p1[2]))
  angle <- calc_angle(p1, p2) + pi/2
  c(point[1] + offset * cos(angle),
    point[2] + offset * sin(angle))
}

#' Create a smooth curve through control points
#' @param x Vector of x coordinates
#' @param y Vector of y coordinates
#' @param n Number of interpolation points
#' @param method Spline method ("natural", "fmm", "periodic")
#' @return Data frame with x, y columns
create_curve <- function(x, y, n = 100, method = "natural") {
  curve <- spline(x = x, y = y, method = method, n = n)
  data.frame(x = curve$x, y = curve$y)
}

#' Divide a line segment into equal parts
#' @param p1 Start point c(x, y)
#' @param p2 End point c(x, y)
#' @param n Number of divisions
#' @return List of n+1 points including endpoints
divide_line <- function(p1, p2, n) {
  lapply(0:n, function(i) {
    c(p1[1] + (i/n) * (p2[1] - p1[1]),
      p1[2] + (i/n) * (p2[2] - p1[2]))
  })
}

#' Calculate bisector point for curve control
#' @param corner Corner point c(x, y)
#' @param p1 First arm point c(x, y)
#' @param p2 Second arm point c(x, y)
#' @param distance Distance along bisector in cm
#' @return Bisector point c(x, y)
bisector_point <- function(corner, p1, p2, distance) {
  angle1 <- calc_angle(corner, p1)
  angle2 <- calc_angle(corner, p2)
  
  # Calculate bisector angle with proper wrapping
  diff <- angle2 - angle1
  if (diff < -pi) diff <- diff + 2*pi
  if (diff > pi) diff <- diff - 2*pi
  bisector <- angle1 + diff/2
  
  c(corner[1] + distance * cos(bisector),
    corner[2] + distance * sin(bisector))
}

#' Find intersection of two lines defined by points
#' @param p1,p2 Points defining first line
#' @param p3,p4 Points defining second line
#' @return Intersection point c(x, y) or NA if parallel
line_intersection <- function(p1, p2, p3, p4) {
  x1 <- p1[1]; y1 <- p1[2]
  x2 <- p2[1]; y2 <- p2[2]
  x3 <- p3[1]; y3 <- p3[2]
  x4 <- p4[1]; y4 <- p4[2]
  
  denom <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  
  if (abs(denom) < 1e-10) return(NA)  # Parallel lines
  
  t <- ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4)) / denom
  
  c(x1 + t*(x2 - x1), y1 + t*(y2 - y1))
}

#' Convert pattern coordinates to full-scale points for printing
#' @param points Data frame with x, y coordinates
#' @param scale Scale factor (1 = full size in cm)
#' @return Data frame with scaled coordinates in mm
to_print_scale <- function(points, scale = 1) {
  points %>%
    mutate(
      x_mm = x * 10 * scale,  # Convert cm to mm
      y_mm = y * 10 * scale
    )
}

#' Validate pattern integrity
#' @param points List of pattern points
#' @param connections List of point pairs that should connect
#' @param tolerance Maximum allowed gap in cm
#' @return Data frame with validation results
validate_pattern <- function(points, connections, tolerance = 0.01) {
  results <- data.frame(
    connection = character(),
    expected = numeric(),
    actual = numeric(),
    pass = logical()
  )
  
  for (conn in names(connections)) {
    pair <- connections[[conn]]
    if (length(pair) == 2 && 
        pair[1] %in% names(points) && 
        pair[2] %in% names(points)) {
      actual <- calc_distance(points[[pair[1]]], points[[pair[2]]])
      results <- rbind(results, data.frame(
        connection = conn,
        actual = round(actual, 3),
        pass = TRUE
      ))
    }
  }
  
  results
}

#' Calculate the total length of a curve
#' @param curve Data frame with x and y columns
#' @return Total length of the curve in cm
curve_length <- function(curve) {
  if (nrow(curve) < 2) return(0)
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}