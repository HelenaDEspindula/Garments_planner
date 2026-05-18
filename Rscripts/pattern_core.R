#' =============================================================================
#' Pattern Core — Funções Geométricas Fundamentais
#' =============================================================================
#' Todas as medidas em centímetros (cm).
#' Coordenadas no formato c(x, y) onde x = horizontal, y = vertical.
#' Origem (0,0) no canto inferior esquerdo do retângulo base.
#' =============================================================================

#' Distância euclidiana entre dois pontos
calc_distance <- function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}

#' Ângulo entre dois pontos (radianos a partir do eixo x positivo)
calc_angle <- function(p1, p2) {
  atan2(p2[2] - p1[2], p2[1] - p1[1])
}

#' Ponto médio entre dois pontos
midpoint <- function(p1, p2) {
  c((p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2)
}

#' Estender uma linha de p1 através de p2 por uma distância
extend_line <- function(p1, p2, distance) {
  angle <- calc_angle(p1, p2)
  c(p2[1] + distance * cos(angle),
    p2[2] + distance * sin(angle))
}

#' Ponto com deslocamento perpendicular a uma linha
perpendicular_offset <- function(p1, p2, offset, position = 0.5) {
  point <- c(p1[1] + position * (p2[1] - p1[1]),
             p1[2] + position * (p2[2] - p1[2]))
  angle <- calc_angle(p1, p2) + pi/2
  c(point[1] + offset * cos(angle),
    point[2] + offset * sin(angle))
}

#' Criar curva suave através de pontos de controle (spline cúbica)
create_curve <- function(x, y, n = 100, method = "natural") {
  curve <- spline(x = x, y = y, method = method, n = n)
  data.frame(x = curve$x, y = curve$y)
}

#' Dividir uma linha em segmentos iguais
divide_line <- function(p1, p2, n) {
  lapply(0:n, function(i) {
    c(p1[1] + (i/n) * (p2[1] - p1[1]),
      p1[2] + (i/n) * (p2[2] - p1[2]))
  })
}

#' Ponto na bissetriz de um ângulo
bisector_point <- function(corner, p1, p2, distance) {
  angle1 <- calc_angle(corner, p1)
  angle2 <- calc_angle(corner, p2)
  
  diff <- angle2 - angle1
  if (diff < -pi) diff <- diff + 2*pi
  if (diff > pi) diff <- diff - 2*pi
  bisector <- angle1 + diff/2
  
  c(corner[1] + distance * cos(bisector),
    corner[2] + distance * sin(bisector))
}

#' Interseção de duas linhas
line_intersection <- function(p1, p2, p3, p4) {
  x1 <- p1[1]; y1 <- p1[2]
  x2 <- p2[1]; y2 <- p2[2]
  x3 <- p3[1]; y3 <- p3[2]
  x4 <- p4[1]; y4 <- p4[2]
  
  denom <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  if (abs(denom) < 1e-10) return(NA)
  
  t <- ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4)) / denom
  c(x1 + t*(x2 - x1), y1 + t*(y2 - y1))
}

#' Comprimento total de uma curva
curve_length <- function(curve) {
  if (nrow(curve) < 2) return(0)
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}

#' Área de um polígono (fórmula de Shoelace)
polygon_area <- function(x, y) {
  n <- length(x)
  if (n < 3) return(0)
  0.5 * abs(sum(x * y[c(2:n, 1)] - x[c(2:n, 1)] * y))
}

#' Converter lista nomeada de pontos para data frame
points_to_df <- function(points, filter_names = NULL) {
  if (!is.null(filter_names)) {
    points <- points[names(points) %in% filter_names]
  }
  data.frame(
    name = names(points),
    x = sapply(points, `[`, 1),
    y = sapply(points, `[`, 2),
    row.names = NULL
  )
}