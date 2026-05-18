#' =============================================================================
#' Blusa (Corpo Simples) — Método Sophia Jobim
#' =============================================================================
#' Baseado em: JOBIM, Sophia. O sistema de corte e costura.
#' Disponível em: https://www.livrosabertos.abcd.usp.br/portaldelivrosUSP/catalog/view/1327/1210/4651
#'
#' Notação de padrões baseada em FreeSewing:
#' https://freesewing.eu/docs/about/notation/
#'
#' Convenções de nomenclatura:
#' - Pontos: point_A, point_B, point_C, ...
#' - Deslocamentos: offset_AE, offset_JM, offset_LO, ...
#' - Curvas: curve_neckline_front, curve_neckline_back, curve_armscye
#' - Medidas: measure_bust, measure_front_length, measure_back_length
#' - Folgas: ease_bust, ease_*
#' - Dimensões: dim_width, dim_height, dim_*
#'
#' Estrutura do output pensada para alimentar funções de modificação futuras:
#' - draft_blouse_sophia_jobim(): traçado base
#' - add_bust_dart(): adicionar pence de busto (futuro)
#' - add_waist_darts(): adicionar pences de cintura (futuro)
#' - modify_neckline(): alterar decote (futuro)
#' =============================================================================

library(here)
source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_notation.R"))
source(here("Rscripts", "pattern_plot.R"))

#' Calculate the total length of a curve
#' @param curve Data frame with x and y columns
#' @return Total length of the curve in cm
curve_length <- function(curve) {
  if (nrow(curve) < 2) return(0)
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}

#' Calculate approximate area of a polygon defined by points
#' Uses Shoelace formula (trapezoid method) - no integrals needed
#' @param x Vector of x coordinates (in order around polygon)
#' @param y Vector of y coordinates (in order around polygon)
#' @return Area in cm²
polygon_area <- function(x, y) {
  n <- length(x)
  if (n < 3) return(0)
  
  # Shoelace formula: 0.5 * |sum(x_i * y_{i+1} - x_{i+1} * y_i)|
  area <- 0.5 * abs(sum(x * y[c(2:n, 1)] - x[c(2:n, 1)] * y))
  return(area)
}

#' Convert named point list to data frame
#' @param points Named list of points c(x, y)
#' @param filter_names Optional vector of names to include (NULL = all)
#' @return Data frame with columns name, x, y
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

#' Draft blouse (corpo simples) — Sophia Jobim method
#'
#' @param measure_bust Circunferência do busto (cm). Ex: 90 para manequim 48
#' @param measure_front_length Comprimento da blusa na frente (cm). Ex: 44
#' @param measure_back_length Comprimento da blusa nas costas (cm). Ex: 41
#' @param ease_bust Aumento na circunferência do busto (cm). Ex: 6
#' @param offset_AE Deslocamento A→E para costas (cm). Ex: 3
#' @param offset_JM Descida do ombro frente J→M (cm). Ex: 4
#' @param offset_AN_extra Profundidade extra decote frente AI+N (cm). Ex: 2
#' @param offset_neckline_shift Desvio lateral do decote (cm). Ex: 1
#' @param offset_B_neck Descida decote costas B→decote (cm). Ex: 2
#' @param offset_KO Altura da pence costas K→O (cm). Ex: 5
#' @param offset_LO_ext Prolongamento LO para P (cm). Ex: 1
#' @param measure_armscye_depth Profundidade da cava (cm). Ex: 21 para braço 33cm
#' @param offset_R_bisector Bissetriz do ângulo R (cm). Ex: 1.5
#' @param offset_S_bisector Bissetriz do ângulo S (cm). Ex: 2.5
#' @param offset_VY Desvio de V para Y (cm). Ex: 2
#' @param offset_XZ Desvio de X para Z (cm). Ex: 0.5
#' @param offset_HW Subida da barra H→W e C→a (cm). Ex: 3
#' @param offset_Wcd Distância de W para c e d (cm). Ex: 2
#'
#' @return Lista com todos os pontos, curvas, peças e metadados
draft_blouse_sophia_jobim <- function(medidas, ease, armscye_depth = NULL) {
  
  # Extrair medidas da lista
  measure_bust <- m_get(medidas, "bust_circ")
  measure_front_length <- m_get(medidas, "neck_front_to_waist_f")
  measure_back_length <- m_get(medidas, "back_waist_length")
  measure_arm_circ <- m_get(medidas, "armscye_circ")
  
  # Extrair parâmetros de ease da lista
  ease_bust <- e_get(ease, "bust_ease")
  offset_AE <- e_get(ease, "side_seam_offset")
  offset_JM <- e_get(ease, "shoulder_slope_front")
  offset_AN_extra <- e_get(ease, "front_neckline_depth_extra")
  offset_neckline_shift <- e_get(ease, "front_neckline_shift")
  offset_B_neck <- e_get(ease, "back_neck_drop")
  offset_KO <- e_get(ease, "back_dart_height")
  offset_LO_ext <- e_get(ease, "back_shoulder_extension")
  offset_R_bisector <- e_get(ease, "bissetriz_R")
  offset_S_bisector <- e_get(ease, "bissetriz_S")
  offset_VY <- e_get(ease, "desvio_V_esquerda")
  offset_XZ <- e_get(ease, "desvio_X_esquerda")
  offset_HW <- e_get(ease, "subida_inferior")
  offset_Wcd <- e_get(ease, "desvio_W_lateral")
  
  # Profundidade da cava: usar valor fornecido ou calcular da tabela
  if (is.null(armscye_depth)) {
    tabela_cava <- read_csv(here("data", "measurements", "sophia_jobim_armscye_table.csv"), 
                            show_col_types = FALSE)
    measure_armscye_depth <- tabela_cava$armscye_depth[tabela_cava$arm_circ == measure_arm_circ]
    if (length(measure_armscye_depth) == 0) {
      measure_armscye_depth <- approx(tabela_cava$arm_circ, tabela_cava$armscye_depth, 
                                      xout = measure_arm_circ)$y
    }
  } else {
    measure_armscye_depth <- armscye_depth
  }
  
  # ===========================================================================
  # Cálculos iniciais
  # ===========================================================================
  # "Corta-se um retângulo de papel ABCD em que a largura AB é a 
  #  circunferência do busto aumentada de 6 cm e o resultado dividido por 2; 
  #  a altura AD, do retângulo, é o comprimento da blusa, tirado pela frente."
  
  dim_width <- (measure_bust + ease_bust) / 2
  dim_height <- measure_front_length
  
  # ===========================================================================
  # PASSO 1: Retângulo Base ABCD
  # ===========================================================================
  point_A <- c(0, dim_height)
  point_B <- c(dim_width, dim_height)
  point_C <- c(dim_width, 0)
  point_D <- c(0, 0)
  
  # ===========================================================================
  # PASSO 2: Deslocamento para Costas (EF)
  # ===========================================================================
  # "A partir dos pontos A e D, passa-se para a direita 3 cm e traça-se a reta EF."
  point_E <- c(point_A[1] + offset_AE, dim_height)
  point_F <- c(point_D[1] + offset_AE, 0)
  
  # ===========================================================================
  # PASSO 3: Linha Central GH (Divisão Frente/Costas)
  # ===========================================================================
  # "Divide-se ao meio EB e FC; chamaremos esses pontos de GH."
  point_G <- c((point_E[1] + point_B[1]) / 2, dim_height)
  point_H <- c((point_F[1] + point_C[1]) / 2, 0)
  
  # ===========================================================================
  # PASSO 4: Divisões da Frente (I, J) e Costas (K, L)
  # ===========================================================================
  # "Dividimos a metade da frente AGHD em quatro partes iguais."
  # "No primeiro 1/4 de AG temos o ponto I"
  # "Aos 3/4 de AG temos o ponto J"
  # "Em seguida dividimos a metade de trás GHCB em quatro partes iguais."
  # "No primeiro 1/4 de GB temos o ponto K"
  # "Aos 3/4 de GB temos o ponto L"
  
  dim_front_division <- (point_G[1] - point_A[1]) / 4
  point_I <- c(point_A[1] + 1 * dim_front_division, dim_height)
  point_J <- c(point_A[1] + 3 * dim_front_division, dim_height)
  
  dim_back_division <- (point_B[1] - point_G[1]) / 4
  point_K <- c(point_G[1] + 1 * dim_back_division, dim_height)
  point_L <- c(point_G[1] + 3 * dim_back_division, dim_height)
  
  # ===========================================================================
  # PASSO 5: Ombro da Frente (M)
  # ===========================================================================
  # "A partir do ponto J marca-se para baixo 4 cm e temos o ponto M. 
  #  Une-se este ponto ao ponto I; a linha IM será a linha do ombro."
  point_M <- c(point_J[1], point_J[2] - offset_JM)
  
  # ===========================================================================
  # PASSO 6: Decote da Frente (N, I2)
  # ===========================================================================
  # "AN é igual a AI mais 2 cm."
  # "O ponto quase sempre precisa ser desviado 1 cm para a direita, 
  #  sobre a linha IM."
  
  distance_AI <- point_I[1] - point_A[1]
  dim_front_neck_depth <- distance_AI + offset_AN_extra
  point_N <- c(point_A[1], point_A[2] - dim_front_neck_depth)
  
  # I2: ponto na linha IM a 1 cm de I em direção a M
  angle_IM <- atan2(point_M[2] - point_I[2], point_M[1] - point_I[1])
  point_I2 <- c(
    point_I[1] + offset_neckline_shift * cos(angle_IM),
    point_I[2] + offset_neckline_shift * sin(angle_IM)
  )
  
  # Curva do decote frente (N → I2, barriga para baixo)
  mid_N_I2 <- midpoint(point_N, point_I2)
  control_N_I2 <- c(mid_N_I2[1] - 1, mid_N_I2[2] - 3)
  curve_neckline_front <- create_curve(
    x = c(point_N[1], control_N_I2[1], point_I2[1]),
    y = c(point_N[2], control_N_I2[2], point_I2[2]),
    n = 50
  )
  
  # ===========================================================================
  # PASSO 7: Decote das Costas
  # ===========================================================================
  # "A partir do ponto B, marca-se para baixo 2 cm e une-se este ao ponto L 
  #  por uma ligeira curva."
  
  point_B_neck <- c(point_B[1], point_B[2] - offset_B_neck)
  
  # L2: ponto na linha LP a 1 cm de L em direção a P
  angle_LP <- atan2(point_P[2] - point_L[2], point_P[1] - point_L[1])
  point_L2 <- c(
    point_L[1] + offset_neckline_shift * cos(angle_LP),
    point_L[2] + offset_neckline_shift * sin(angle_LP)
  )
  
  # Curva do decote costas (B_neck → L2, barriga para baixo)
  mid_B_L2 <- midpoint(point_B_neck, point_L2)
  control_B_L2 <- c(mid_B_L2[1] + 1, mid_B_L2[2] - 2)
  curve_neckline_back <- create_curve(
    x = c(point_B_neck[1], control_B_L2[1], point_L2[1]),
    y = c(point_B_neck[2], control_B_L2[2], point_L2[2]),
    n = 50
  )
  
  # ===========================================================================
  # PASSO 8: Ombro das Costas (O, P)
  # ===========================================================================
  # "Marca-se de K para baixo 5 cm e temos o ponto O. 
  #  Une-se L a O e prolonga-se esta reta de 1 cm. Temos o ponto P."
  
  point_O <- c(point_K[1], point_K[2] - offset_KO)
  angle_LO <- atan2(point_O[2] - point_L[2], point_O[1] - point_L[1])
  point_P <- c(
    point_O[1] + offset_LO_ext * cos(angle_LO),
    point_O[2] + offset_LO_ext * sin(angle_LO)
  )
  
  # ===========================================================================
  # PASSO 9: Linha da Cava (R, S, Q)
  # ===========================================================================
  # "Marca-se 21 cm para baixo de J e de K e teremos os pontos R e S."
  # "A reta RS determina o ponto Q sobre a divisão GH."
  
  point_R <- c(point_J[1], point_J[2] - measure_armscye_depth)
  point_S <- c(point_K[1], point_K[2] - measure_armscye_depth)
  point_Q <- c(point_G[1], point_R[2])
  
  # ===========================================================================
  # PASSO 10: Bissetrizes (T, U)
  # ===========================================================================
  # "Tira-se as bissetrizes dos ângulos R e S."
  
  point_T <- c(
    point_R[1] + offset_R_bisector * cos(pi / 4),
    point_R[2] + offset_R_bisector * sin(pi / 4)
  )
  point_U <- c(
    point_S[1] + offset_S_bisector * cos(3 * pi / 4),
    point_S[2] + offset_S_bisector * sin(3 * pi / 4)
  )
  
  # ===========================================================================
  # PASSO 11: Pontos Médios e Desvios (V, X, Y, Z)
  # ===========================================================================
  # "Divide-se as distâncias MR e OS ao meio e temos os pontos VX."
  # "Para a esquerda de V marca-se 2 cm (ponto Y)"
  # "Para a esquerda de X marca-se meio cm (ponto Z)"
  
  point_V <- midpoint(point_M, point_R)
  point_X <- midpoint(point_O, point_S)
  point_Y <- c(point_V[1] - offset_VY, point_V[2])
  point_Z <- c(point_X[1] - offset_XZ, point_X[2])
  
  # ===========================================================================
  # PASSO 12: Curva da Cava (Spline Cúbica Paramétrica)
  # ===========================================================================
  # "Unindo-se por uma curva os pontos P, Z, U, Q, T, Y, M teremos a cava."
  # Método: Spline cúbica natural paramétrica
  
  cava_x <- c(point_P[1], point_Z[1], point_U[1], point_Q[1], 
              point_T[1], point_Y[1], point_M[1])
  cava_y <- c(point_P[2], point_Z[2], point_U[2], point_Q[2], 
              point_T[2], point_Y[2], point_M[2])
  
  cava_t <- c(0, cumsum(sqrt(diff(cava_x)^2 + diff(cava_y)^2)))
  unique_idx <- !duplicated(cava_t)
  cava_t_unique <- cava_t[unique_idx]
  cava_x_unique <- cava_x[unique_idx]
  cava_y_unique <- cava_y[unique_idx]
  
  curve_armscye <- data.frame(
    x = spline(cava_t_unique, cava_x_unique, method = "natural", n = 150)$y,
    y = spline(cava_t_unique, cava_y_unique, method = "natural", n = 150)$y
  )
  
  # ===========================================================================
  # PASSO 13: Parte Inferior (W, a, b, c, d)
  # ===========================================================================
  # "Marca-se 3 cm para cima dos pontos H e C e liga-se esses dois pontos (aw)."
  # "Em seguida, une-se W ao ponto b, pé da divisão I."
  # "Marca-se 2 cm para a direita e para a esquerda do ponto W; 
  #  temos os pontos c e d."
  # 
  # "A figura I-M-Y-T-Q-c-b-D-N-I é o molde da metade da frente da blusa; 
  #  L-P-Z-U-Q-d-a-B'-L é o molde da metade das costas."
  
  point_W <- c(point_H[1], point_H[2] + offset_HW)
  point_a <- c(point_C[1], point_C[2] + offset_HW)
  point_b <- c(point_I[1], 0)
  point_c <- c(point_W[1] - offset_Wcd, point_W[2])
  point_d <- c(point_W[1] + offset_Wcd, point_W[2])
  
  # ===========================================================================
  # MONTAGEM DAS PEÇAS
  # ===========================================================================
  # FRENTE: I → M → Y → T → Q → c → b → D → N → I
  # COSTAS: L → P → Z → U → Q → d → a → B' → L
  
  front_piece <- list(
    name = "Frente",
    center_front = list(from = point_D, to = point_N),
    neckline = list(from = point_N, to = point_I2),
    shoulder = list(from = point_I2, to = point_M),
    armscye_part = list(from = point_M, to = point_Q, via = c("Y", "T")),
    side_seam = list(from = point_Q, to = point_c),
    waist = list(from = point_c, to = point_b),
    hem = list(from = point_b, to = point_D)
  )
  
  back_piece <- list(
    name = "Costas",
    shoulder = list(from = point_L2, to = point_P),
    armscye_part = list(from = point_P, to = point_Q, via = c("Z", "U")),
    side_seam = list(from = point_Q, to = point_d),
    waist = list(from = point_d, to = point_a),
    center_back = list(from = point_a, to = point_B_neck),
    neckline = list(from = point_B_neck, to = point_L2)
  )
  
  # ===========================================================================
  # CÁLCULO DE ÁREA APROXIMADA (sem curvas, usando pontos diretos)
  # ===========================================================================
  # Frente: polígono I-M-Y-T-Q-c-b-D-N-I (aproximação linear)
  front_area <- polygon_area(
    x = c(point_I[1], point_M[1], point_Y[1], point_T[1], point_Q[1], 
          point_c[1], point_b[1], point_D[1], point_N[1]),
    y = c(point_I[2], point_M[2], point_Y[2], point_T[2], point_Q[2], 
          point_c[2], point_b[2], point_D[2], point_N[2])
  )
  
  # Costas: polígono L-P-Z-U-Q-d-a-B'-L (aproximação linear)
  back_area <- polygon_area(
    x = c(point_L[1], point_P[1], point_Z[1], point_U[1], point_Q[1], 
          point_d[1], point_a[1], point_B_neck[1]),
    y = c(point_L[2], point_P[2], point_Z[2], point_U[2], point_Q[2], 
          point_d[2], point_a[2], point_B_neck[2])
  )
  
  # ===========================================================================
  # COLETA DE TODOS OS PONTOS
  # ===========================================================================
  all_points <- list(
    point_A = point_A, point_B = point_B, point_C = point_C, point_D = point_D,
    point_E = point_E, point_F = point_F,
    point_G = point_G, point_H = point_H,
    point_I = point_I, point_J = point_J, point_K = point_K, point_L = point_L,
    point_M = point_M, point_N = point_N, point_O = point_O, point_P = point_P,
    point_Q = point_Q, point_R = point_R, point_S = point_S,
    point_T = point_T, point_U = point_U,
    point_V = point_V, point_X = point_X, point_Y = point_Y, point_Z = point_Z,
    point_W = point_W, point_a = point_a, point_b = point_b, 
    point_c = point_c, point_d = point_d,
    point_B_neck = point_B_neck,
    point_I2 = point_I2,
    point_L2 = point_L2
  )
  
  # ===========================================================================
  # COMPRIMENTOS DAS LINHAS
  # ===========================================================================
  line_lengths <- list(
    shoulder_front_IM = calc_distance(point_I, point_M),
    neckline_front = curve_length(curve_neckline_front),
    shoulder_back_LP = calc_distance(point_L, point_P),
    neckline_back = curve_length(curve_neckline_back),
    armscye = curve_length(curve_armscye),
    side_front_Qc = calc_distance(point_Q, point_c),
    side_back_Qd = calc_distance(point_Q, point_d),
    waist_front_cb = calc_distance(point_c, point_b),
    waist_back_da = calc_distance(point_d, point_a),
    hem_front_bD = calc_distance(point_b, point_D),
    center_front_DN = calc_distance(point_D, point_N),
    center_back_aB = calc_distance(point_a, point_B_neck)
  )
  
  # ===========================================================================
  # METADADOS
  # ===========================================================================
  measurements_used <- list(
    measure_bust = measure_bust,
    measure_front_length = measure_front_length,
    measure_back_length = measure_back_length
  )
  
  ease_used <- list(
    ease_bust = ease_bust,
    offset_AE = offset_AE,
    offset_JM = offset_JM,
    offset_AN_extra = offset_AN_extra,
    offset_neckline_shift = offset_neckline_shift,
    offset_B_neck = offset_B_neck,
    offset_KO = offset_KO,
    offset_LO_ext = offset_LO_ext,
    measure_armscye_depth = measure_armscye_depth,
    offset_R_bisector = offset_R_bisector,
    offset_S_bisector = offset_S_bisector,
    offset_VY = offset_VY,
    offset_XZ = offset_XZ,
    offset_HW = offset_HW,
    offset_Wcd = offset_Wcd
  )
  
  # ===========================================================================
  # RETORNO
  # ===========================================================================
  list(
    points = all_points,
    curves = list(
      curve_neckline_front = curve_neckline_front,
      curve_neckline_back = curve_neckline_back,
      curve_armscye = curve_armscye
    ),
    pieces = list(
      front = front_piece,
      back = back_piece
    ),
    line_lengths = line_lengths,
    area = list(
      front_area_cm2 = front_area,
      back_area_cm2 = back_area,
      total_area_cm2 = front_area + back_area,
      front_area_m2 = front_area / 10000,
      back_area_m2 = back_area / 10000,
      total_area_m2 = (front_area + back_area) / 10000,
      note = "Area aproximada calculada por poligono sem considerar curvas. Para estimativa de tecido, adicionar 10-15% para margens e curvas."
    ),
    dimensions = list(
      dim_width = dim_width,
      dim_height = dim_height,
      dim_front_division = dim_front_division,
      dim_back_division = dim_back_division,
      dim_front_neck_depth = dim_front_neck_depth
    ),
    measurements = measurements_used,
    ease = ease_used
  )
}

# =============================================================================
# TESTE RÁPIDO
# =============================================================================
if (FALSE) {
  source(here("Rscripts", "blouse_sophia_jobim.R"))
  
  blusa <- draft_blouse_sophia_jobim()
  
  # Verificar comprimentos
  str(blusa$line_lengths)
  
  # Verificar áreas
  cat(sprintf("\nArea frente: %.0f cm² (%.2f m²)\n", 
              blusa$area$front_area_cm2, blusa$area$front_area_m2))
  cat(sprintf("Area costas: %.0f cm² (%.2f m²)\n", 
              blusa$area$back_area_cm2, blusa$area$back_area_m2))
  cat(sprintf("Area total:  %.0f cm² (%.2f m²)\n", 
              blusa$area$total_area_cm2, blusa$area$total_area_m2))
  cat(sprintf("%s\n", blusa$area$note))
  
  # Plot diagnóstico
  pts <- blusa$points
  plot(0, 0, type = "n", asp = 1,
       xlim = c(-5, 55), ylim = c(-5, 55),
       xlab = "x (cm)", ylab = "y (cm)",
       main = "Blusa Sophia Jobim - Area aproximada")
  
  rect(0, 0, blusa$dimensions$dim_width, blusa$dimensions$dim_height,
       border = "grey80", lty = 2)
  
  abline(v = pts$point_E[1], col = "grey60", lty = 3)
  abline(v = pts$point_G[1], col = "grey60", lty = 3)
  abline(h = pts$point_R[2], col = "grey60", lty = 3)
  
  for (name in names(pts)) {
    pt <- pts[[name]]
    points(pt[1], pt[2], pch = 16, cex = 1.2, col = "red")
    text(pt[1], pt[2], name, pos = 3, cex = 0.6, col = "darkred")
  }
  
  lines(blusa$curves$curve_neckline_front, col = "#2196F3", lwd = 2)
  lines(blusa$curves$curve_neckline_back, col = "#FF9800", lwd = 2)
  lines(blusa$curves$curve_armscye, col = "#333333", lwd = 2)
  
  # Conexões frente
  lines(c(pts$point_I[1], pts$point_M[1]), c(pts$point_I[2], pts$point_M[2]), col = "#2196F3", lwd = 2)
  lines(c(pts$point_Q[1], pts$point_c[1]), c(pts$point_Q[2], pts$point_c[2]), col = "#2196F3", lwd = 2)
  lines(c(pts$point_c[1], pts$point_b[1]), c(pts$point_c[2], pts$point_b[2]), col = "#2196F3", lwd = 2)
  lines(c(pts$point_b[1], pts$point_D[1]), c(pts$point_b[2], pts$point_D[2]), col = "#2196F3", lwd = 2)
  lines(c(pts$point_D[1], pts$point_N[1]), c(pts$point_D[2], pts$point_N[2]), col = "#2196F3", lwd = 2)
  
  # Conexões costas
  lines(c(pts$point_L[1], pts$point_P[1]), c(pts$point_L[2], pts$point_P[2]), col = "#FF9800", lwd = 2)
  lines(c(pts$point_Q[1], pts$point_d[1]), c(pts$point_Q[2], pts$point_d[2]), col = "#FF9800", lwd = 2)
  lines(c(pts$point_d[1], pts$point_a[1]), c(pts$point_d[2], pts$point_a[2]), col = "#FF9800", lwd = 2)
  lines(c(pts$point_a[1], pts$point_B_neck[1]), c(pts$point_a[2], pts$point_B_neck[2]), col = "#FF9800", lwd = 2)
  
  legend("topright", 
         legend = c("Frente", "Costas", "Cava"),
         col = c("#2196F3", "#FF9800", "#333333"),
         lwd = 2, cex = 0.8)
}