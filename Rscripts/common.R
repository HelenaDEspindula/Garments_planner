# Rscripts/common.R - Atualizado
library(here)

# Criar diretórios necessários
dir.create(here("images", "cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), showWarnings = FALSE)

# Carregar módulos na ordem correta
source(here("Rscripts", "pattern_core.R"))
source(here("Rscripts", "pattern_notation.R"))
source(here("Rscripts", "pattern_measurements.R"))
source(here("Rscripts", "pattern_blocks.R"))
source(here("Rscripts", "pattern_modifiers.R"))
source(here("Rscripts", "pattern_area.R"))
source(here("Rscripts", "pattern_plot.R"))
source(here("Rscripts", "pattern_export.R"))
source(here("Rscripts", "pattern_animation.R"))