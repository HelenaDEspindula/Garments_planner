#' =============================================================================
#' Traçado Interativo — Blusa (Corpo Simples) — Sophia Jobim
#' =============================================================================
#' Execute linha por linha (Ctrl+Enter) para ver cada passo do traçado.
#' O gráfico acumula elementos a cada passo.
#' 
#' Modo de uso:
#' 1. Execute o bloco "Setup" primeiro
#' 2. Depois vá executando cada PASSO em sequência
#' 3. O plot se atualiza automaticamente após cada passo
#' =============================================================================

library(here)
source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_notation.R"))

# =============================================================================
# SETUP: Carregar medidas e preparar o ambiente
# =============================================================================
source(here("Rscripts", "measurement_io.R"))

# Carregar dados
medidas_sj <- read_measurements(here("data", "measurements", "sophia_jobim_size48.csv"))
ease_sj <- read_ease(here("data", "parameters", "sophia_jobim_ease.csv"))

# Extrair medidas
measure_bust <- m_get(medidas_sj, "bust_circ")
measure_front_length <- m_get(medidas_sj, "neck_front_to_waist_f")
measure_back_length <- m_get(medidas_sj, "back_waist_length")
measure_arm_circ <- m_get(medidas_sj, "armscye_circ")

# Extrair parâmetros
ease_bust <- e_get(ease_sj, "bust_ease")
offset_AE <- e_get(ease_sj, "side_seam_offset")
offset_JM <- e_get(ease_sj, "shoulder_slope_front")
offset_AN_extra <- e_get(ease_sj, "front_neckline_depth_extra")
offset_neckline_shift <- e_get(ease_sj, "front_neckline_shift")
offset_B_neck <- e_get(ease_sj, "back_neck_drop")
offset_KO <- e_get(ease_sj, "back_dart_height")
offset_LO_ext <- e_get(ease_sj, "back_shoulder_extension")
offset_R_bisector <- e_get(ease_sj, "bissetriz_R")
offset_S_bisector <- e_get(ease_sj, "bissetriz_S")
offset_VY <- e_get(ease_sj, "desvio_V_esquerda")
offset_XZ <- e_get(ease_sj, "desvio_X_esquerda")
offset_HW <- e_get(ease_sj, "subida_inferior")
offset_Wcd <- e_get(ease_sj, "desvio_W_lateral")

# Tabela de cava
tabela_cava <- read_csv(here("data", "measurements", "sophia_jobim_armscye_table.csv"), 
                        show_col_types = FALSE)
measure_armscye_depth <- tabela_cava$armscye_depth[tabela_cava$arm_circ == measure_arm_circ]
if (length(measure_armscye_depth) == 0) {
  measure_armscye_depth <- approx(tabela_cava$arm_circ, tabela_cava$armscye_depth, 
                                  xout = measure_arm_circ)$y
}

# Dimensões do retângulo
dim_width <- (measure_bust + ease_bust) / 2
dim_height <- measure_front_length

# Função auxiliar para criar o plot base
create_base_plot <- function(title = "Traçado Interativo — Sophia Jobim") {
  plot(0, 0, type = "n", asp = 1,
       xlim = c(-5, dim_width + 5), 
       ylim = c(-5, dim_height + 5),
       xlab = "x (cm)", ylab = "y (cm)",
       main = title)
  grid(col = "grey90", lty = 3)
}

# Função para adicionar ponto com label
add_point <- function(pt, label = NULL, col = "red", cex = 1.5) {
  points(pt[1], pt[2], pch = 16, cex = cex, col = col)
  if (!is.null(label)) {
    text(pt[1], pt[2], label, pos = 3, cex = 0.9, col = col, font = 2)
  }
}

# Função para adicionar linha
add_line <- function(from, to, col = "black", lwd = 2, lty = 1) {
  lines(c(from[1], to[1]), c(from[2], to[2]), col = col, lwd = lwd, lty = lty)
}

# Função para adicionar curva
add_curve <- function(curve, col = "black", lwd = 2) {
  lines(curve$x, curve$y, col = col, lwd = lwd)
}

# Função para calcular comprimento de curva
curve_length <- function(curve) {
  if (nrow(curve) < 2) return(0)
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}

cat("\n========================================\n")
cat("  Ambiente pronto. Execute os passos.\n")
cat("========================================\n")
cat(sprintf("  Busto: %.0f cm | Frente: %.0f cm | Costas: %.0f cm\n", 
            measure_bust, measure_front_length, measure_back_length))
cat(sprintf("  Largura do molde: %.1f cm | Altura: %.0f cm\n", 
            dim_width, dim_height))
cat(sprintf("  Profundidade da cava: %.0f cm (braco %.0f cm)\n", 
            measure_armscye_depth, measure_arm_circ))


# =============================================================================
# PASSO 1: Retangulo Base ABCD
# =============================================================================
# "Corta-se um retangulo de papel ABCD em que a largura AB e a 
#  circunferencia do busto aumentada de 6 cm e o resultado dividido por 2; 
#  a altura AD, do retangulo, e o comprimento da blusa, tirado pela frente."

point_A <- c(0, dim_height)
point_B <- c(dim_width, dim_height)
point_C <- c(dim_width, 0)
point_D <- c(0, 0)

# Plot
create_base_plot("Passo 1: Retangulo Base ABCD")
rect(0, 0, dim_width, dim_height, border = "grey50", lty = 2)
add_point(point_A, "A", col = "grey50")
add_point(point_B, "B", col = "grey50")
add_point(point_C, "C", col = "grey50")
add_point(point_D, "D", col = "grey50")

cat(sprintf("\nPASSO 1: Retangulo %.1f x %.0f cm\n", dim_width, dim_height))


# =============================================================================
# PASSO 2: Deslocamento para Costas (EF)
# =============================================================================
# "A partir dos pontos A e D, passa-se para a direita 3 cm e traca-se a reta EF."

point_E <- c(point_A[1] + offset_AE, dim_height)
point_F <- c(point_D[1] + offset_AE, 0)

add_line(point_E, point_F, col = "#377EB8", lwd = 2, lty = 5)
add_point(point_E, "E", col = "#377EB8")
add_point(point_F, "F", col = "#377EB8")

text(point_A[1] + offset_AE/2, dim_height + 2, 
     paste(offset_AE, "cm"), cex = 0.8, col = "#377EB8")

cat(sprintf("PASSO 2: Deslocamento EF = %.0f cm\n", offset_AE))


# =============================================================================
# PASSO 3: Linha Central GH
# =============================================================================
# "Divide-se ao meio EB e FC; chamaremos esses pontos de GH."

point_G <- c((point_E[1] + point_B[1]) / 2, dim_height)
point_H <- c((point_F[1] + point_C[1]) / 2, 0)

add_line(point_G, point_H, col = "#4DAF4A", lwd = 2, lty = 4)
add_point(point_G, "G", col = "#4DAF4A")
add_point(point_H, "H", col = "#4DAF4A")

text(point_G[1] + 1, dim_height/2, "Linha\nda cava", 
     cex = 0.7, col = "#4DAF4A")

cat(sprintf("PASSO 3: Centro G (%.1f, %.0f) | H (%.1f, 0)\n", 
            point_G[1], point_G[2], point_H[1]))


# =============================================================================
# PASSO 4: Divisoes da Frente (I, J) e Costas (K, L)
# =============================================================================
# "Dividimos a metade da frente AGHD em quatro partes iguais."
# "No primeiro 1/4 de AG temos o ponto I"
# "Aos 3/4 de AG temos o ponto J"
# "Em seguida dividimos a metade de tras GHCB em quatro partes iguais."
# "No primeiro 1/4 de GB temos o ponto K"
# "Aos 3/4 de GB temos o ponto L"

dim_front_division <- (point_G[1] - point_A[1]) / 4
point_I <- c(point_A[1] + 1 * dim_front_division, dim_height)
point_J <- c(point_A[1] + 3 * dim_front_division, dim_height)

dim_back_division <- (point_B[1] - point_G[1]) / 4
point_K <- c(point_G[1] + 1 * dim_back_division, dim_height)
point_L <- c(point_G[1] + 3 * dim_back_division, dim_height)

for (pt in list(point_I, point_J)) {
  lines(c(pt[1], pt[1]), c(0, dim_height), col = "grey80", lty = 3)
}
for (pt in list(point_K, point_L)) {
  lines(c(pt[1], pt[1]), c(0, dim_height), col = "grey80", lty = 3)
}

add_point(point_I, "I", col = "#FF7F00")
add_point(point_J, "J", col = "#FF7F00")
add_point(point_K, "K", col = "#FF7F00")
add_point(point_L, "L", col = "#FF7F00")

cat(sprintf("PASSO 4: Frente I=%.1f J=%.1f | Costas K=%.1f L=%.1f\n", 
            point_I[1], point_J[1], point_K[1], point_L[1]))
cat(sprintf("         Divisao frente: %.1f cm | Divisao costas: %.1f cm\n", 
            dim_front_division, dim_back_division))


# =============================================================================
# PASSO 5: Ombro da Frente (M)
# =============================================================================
# "A partir do ponto J marca-se para baixo 4 cm e temos o ponto M."

point_M <- c(point_J[1], point_J[2] - offset_JM)

add_line(point_J, point_M, col = "grey70", lwd = 1, lty = 3)
add_line(point_I, point_M, col = "#2196F3", lwd = 2.5)
add_point(point_M, "M", col = "#2196F3", cex = 2)

dist_IM <- round(sqrt((point_M[1]-point_I[1])^2 + (point_M[2]-point_I[2])^2), 1)
text(mean(c(point_I[1], point_M[1])), mean(c(point_I[2], point_M[2])) - 1,
     paste(dist_IM, "cm"), cex = 0.7, col = "#2196F3")

cat(sprintf("PASSO 5: Ombro frente IM = %.1f cm\n", dist_IM))


# =============================================================================
# PASSO 6: Decote da Frente (N, I2)
# =============================================================================
# "AN e igual a AI mais 2 cm. Traca-se a curva IN."
# "O ponto quase sempre precisa ser desviado 1 cm para a direita, 
#  sobre a linha IM."

distance_AI <- point_I[1] - point_A[1]
dim_front_neck_depth <- distance_AI + offset_AN_extra
point_N <- c(point_A[1], point_A[2] - dim_front_neck_depth)

angle_IM <- atan2(point_M[2] - point_I[2], point_M[1] - point_I[1])
point_I2 <- c(
  point_I[1] + offset_neckline_shift * cos(angle_IM),
  point_I[2] + offset_neckline_shift * sin(angle_IM)
)

mid_N_I2 <- c((point_N[1] + point_I2[1]) / 2, (point_N[2] + point_I2[2]) / 2)
control_N_I2 <- c(mid_N_I2[1] - 1, mid_N_I2[2] - 3)

curve_neckline_front <- create_curve(
  x = c(point_N[1], control_N_I2[1], point_I2[1]),
  y = c(point_N[2], control_N_I2[2], point_I2[2]),
  n = 50
)

add_curve(curve_neckline_front, col = "#2196F3", lwd = 2.5)
add_point(point_N, "N", col = "#2196F3")
add_point(point_I2, "I2", col = "#2196F3", cex = 1.5)

add_line(point_I, point_I2, col = "grey70", lwd = 1, lty = 3)
text(point_I[1] + offset_neckline_shift/2, point_I[2] + 0.5,
     paste(offset_neckline_shift, "cm"), cex = 0.6, col = "grey50")

lines(c(point_A[1], point_A[1]), c(point_A[2], point_N[2]), col = "grey70", lty = 3)
text(point_A[1] - 1.5, mean(c(point_A[2], point_N[2])), 
     paste(dim_front_neck_depth, "cm"), cex = 0.7, srt = 90, col = "grey50")

cat(sprintf("PASSO 6: Decote frente AN = %.1f cm | I->I2 = %.0f cm\n", 
            dim_front_neck_depth, offset_neckline_shift))


# =============================================================================
# PASSO 7: Decote das Costas
# =============================================================================
# "A partir do ponto B, marca-se para baixo 2 cm e une-se este ao ponto L."

point_B_neck <- c(point_B[1], point_B[2] - offset_B_neck)

angle_LP <- atan2(point_P[2] - point_L[2], point_P[1] - point_L[1])
point_L2 <- c(
  point_L[1] + offset_neckline_shift * cos(angle_LP),
  point_L[2] + offset_neckline_shift * sin(angle_LP)
)

mid_B_L2 <- c((point_B_neck[1] + point_L2[1]) / 2, 
              (point_B_neck[2] + point_L2[2]) / 2)
control_B_L2 <- c(mid_B_L2[1] + 1, mid_B_L2[2] - 2)

curve_neckline_back <- create_curve(
  x = c(point_B_neck[1], control_B_L2[1], point_L2[1]),
  y = c(point_B_neck[2], control_B_L2[2], point_L2[2]),
  n = 50
)

add_curve(curve_neckline_back, col = "#FF9800", lwd = 2.5)
add_point(point_B_neck, "B'", col = "#FF9800")
add_point(point_L2, "L2", col = "#FF9800", cex = 1.5)

add_line(point_L, point_L2, col = "grey70", lwd = 1, lty = 3)

cat(sprintf("PASSO 7: Decote costas descida = %.0f cm | L->L2 = %.0f cm\n", 
            offset_B_neck, offset_neckline_shift))


# =============================================================================
# PASSO 8: Ombro das Costas (O, P)
# =============================================================================
# "Marca-se de K para baixo 5 cm (ponto O). Une-se L a O e prolonga-se 1 cm (ponto P)."

point_O <- c(point_K[1], point_K[2] - offset_KO)
angle_LO <- atan2(point_O[2] - point_L[2], point_O[1] - point_L[1])
point_P <- c(
  point_O[1] + offset_LO_ext * cos(angle_LO),
  point_O[2] + offset_LO_ext * sin(angle_LO)
)

add_line(point_K, point_O, col = "grey70", lwd = 1, lty = 3)
add_line(point_L, point_P, col = "#FF9800", lwd = 2.5)
add_point(point_O, "O", col = "#FF9800")
add_point(point_P, "P", col = "#FF9800", cex = 2)

dist_LP <- round(sqrt((point_P[1]-point_L[1])^2 + (point_P[2]-point_L[2])^2), 1)
text(mean(c(point_L[1], point_P[1])), mean(c(point_L[2], point_P[2])) + 1.5,
     paste(dist_LP, "cm"), cex = 0.7, col = "#FF9800")

cat(sprintf("PASSO 8: Ombro costas LP = %.1f cm\n", dist_LP))


# =============================================================================
# PASSO 9: Linha da Cava (R, S, Q)
# =============================================================================
# "Marca-se 21 cm para baixo de J e de K (pontos R e S)."

point_R <- c(point_J[1], point_J[2] - measure_armscye_depth)
point_S <- c(point_K[1], point_K[2] - measure_armscye_depth)
point_Q <- c(point_G[1], point_R[2])

add_line(point_R, point_S, col = "grey70", lwd = 1, lty = 3)
add_point(point_R, "R", col = "#984EA3")
add_point(point_S, "S", col = "#984EA3")
add_point(point_Q, "Q", col = "#984EA3")

text(point_J[1] + 1, point_R[2] + measure_armscye_depth/2,
     paste(measure_armscye_depth, "cm"), cex = 0.7, srt = 90, col = "grey50")

cat(sprintf("PASSO 9: Cava a %.0f cm de profundidade\n", measure_armscye_depth))


# =============================================================================
# PASSO 10: Bissetrizes (T, U)
# =============================================================================
# "Bissetrizes: 1.5 cm em R, 2.5 cm em S."

point_T <- c(
  point_R[1] + offset_R_bisector * cos(pi/4),
  point_R[2] + offset_R_bisector * sin(pi/4)
)
point_U <- c(
  point_S[1] + offset_S_bisector * cos(3*pi/4),
  point_S[2] + offset_S_bisector * sin(3*pi/4)
)

add_line(point_R, point_T, col = "grey85", lwd = 1, lty = 3)
add_line(point_S, point_U, col = "grey85", lwd = 1, lty = 3)

add_point(point_T, "T", col = "#A65628")
add_point(point_U, "U", col = "#A65628")

cat(sprintf("PASSO 10: T (bissetriz R = %.1f cm) | U (bissetriz S = %.1f cm)\n", 
            offset_R_bisector, offset_S_bisector))


# =============================================================================
# PASSO 11: Pontos Medios (V, X, Y, Z)
# =============================================================================
# "MR e OS ao meio = V, X. Esquerda de V 2cm = Y. Esquerda de X 0.5cm = Z."

point_V <- midpoint(point_M, point_R)
point_X <- midpoint(point_O, point_S)
point_Y <- c(point_V[1] - offset_VY, point_V[2])
point_Z <- c(point_X[1] - offset_XZ, point_X[2])

add_point(point_V, "V", col = "grey60", cex = 1)
add_point(point_X, "X", col = "grey60", cex = 1)
add_point(point_Y, "Y", col = "#F781BF", cex = 1.5)
add_point(point_Z, "Z", col = "#F781BF", cex = 1.5)

add_line(point_V, point_Y, col = "grey85", lwd = 1, lty = 3)
add_line(point_X, point_Z, col = "grey85", lwd = 1, lty = 3)

cat(sprintf("PASSO 11: V->Y = %.0f cm | X->Z = %.1f cm\n", offset_VY, offset_XZ))


# =============================================================================
# PASSO 12: Curva da Cava (Spline Cubica Parametrica)
# =============================================================================
# "Unindo P, Z, U, Q, T, Y, M teremos a cava da manga."
# Metodo: Spline cubica natural parametrica

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

add_curve(curve_armscye, col = "#333333", lwd = 3)

cava_labels <- c("P", "Z", "U", "Q", "T", "Y", "M")
for (i in seq_along(cava_labels)) {
  points(cava_x[i], cava_y[i], pch = 21, bg = "white", 
         cex = 1.8, col = "#333333", lwd = 2.5)
  text(cava_x[i], cava_y[i], cava_labels[i], pos = 3, 
       col = "#333333", font = 2, cex = 1)
}

armscye_length <- round(curve_length(curve_armscye), 1)
text(point_Q[1] + 3, point_Q[2] + 2,
     paste("Cava:", armscye_length, "cm"), cex = 0.9, col = "#333333", font = 2)

cat(sprintf("PASSO 12: Cava (Spline natural) = %.1f cm\n", armscye_length))


# =============================================================================
# PASSO 13: Parte Inferior (W, a, b, c, d)
# =============================================================================
# "Marca-se 3 cm para cima de H e C = aw. W ao ponto b (pe de I).
#  2 cm esq/dir de W = c, d."

point_W <- c(point_H[1], point_H[2] + offset_HW)
point_a <- c(point_C[1], point_C[2] + offset_HW)
point_b <- c(point_I[1], 0)
point_c <- c(point_W[1] - offset_Wcd, point_W[2])
point_d <- c(point_W[1] + offset_Wcd, point_W[2])

add_line(point_W, point_a, col = "grey70", lwd = 1, lty = 3)
add_point(point_W, "W", col = "#66C2A5")
add_point(point_a, "a", col = "#66C2A5")
add_point(point_b, "b", col = "#2196F3")
add_point(point_c, "c", col = "#2196F3")
add_point(point_d, "d", col = "#FF9800")

cat(sprintf("PASSO 13: Parte inferior (W, a, b, c, d)\n"))


# =============================================================================
# PASSO 14: Conexoes Finais — FRENTE
# =============================================================================
# "I-M-Y-T-Q-c-b-D-N-I e o molde da metade da frente."

add_line(point_Q, point_c, col = "#2196F3", lwd = 2.5)
add_line(point_c, point_b, col = "#2196F3", lwd = 2.5)
add_line(point_b, point_D, col = "#2196F3", lwd = 2.5)
add_line(point_D, point_N, col = "#2196F3", lwd = 2.5)

cat("PASSO 14: Frente conectada (I-M-Y-T-Q-c-b-D-N-I)\n")


# =============================================================================
# PASSO 15: Conexoes Finais — COSTAS
# =============================================================================
# "L-P-Z-U-Q-d-a-B'-L e o molde da metade das costas."

add_line(point_Q, point_d, col = "#FF9800", lwd = 2.5)
add_line(point_d, point_a, col = "#FF9800", lwd = 2.5)
add_line(point_a, point_B_neck, col = "#FF9800", lwd = 2.5)

cat("PASSO 15: Costas conectada (L-P-Z-U-Q-d-a-B'-L)\n")


# =============================================================================
# RESUMO FINAL
# =============================================================================

text(point_I[1] + 2, dim_height/2, "FRENTE", 
     cex = 2, col = "#2196F3", font = 2, srt = 90)
text(point_d[1] + 3, dim_height/2, "COSTAS", 
     cex = 2, col = "#FF9800", font = 2, srt = 90)

cat("\n========================================\n")
cat("  COMPRIMENTOS FINAIS\n")
cat("========================================\n")
cat(sprintf("  Ombro frente (I-M):      %.1f cm\n", dist_IM))
cat(sprintf("  Ombro costas (L-P):      %.1f cm\n", dist_LP))
cat(sprintf("  Cava (P-M):              %.1f cm\n", armscye_length))
cat(sprintf("  Lateral frente (Q-c):    %.1f cm\n", 
            round(sqrt((point_Q[1]-point_c[1])^2 + (point_Q[2]-point_c[2])^2), 1)))
cat(sprintf("  Lateral costas (Q-d):    %.1f cm\n", 
            round(sqrt((point_Q[1]-point_d[1])^2 + (point_Q[2]-point_d[2])^2), 1)))
cat(sprintf("  Centro frente (D-N):     %.1f cm\n", 
            round(sqrt((point_D[1]-point_N[1])^2 + (point_D[2]-point_N[2])^2), 1)))
cat(sprintf("  Centro costas (a-B'):    %.1f cm\n", 
            round(sqrt((point_a[1]-point_B_neck[1])^2 + (point_a[2]-point_B_neck[2])^2), 1)))
cat("========================================\n")