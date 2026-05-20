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
#source(here("Rscripts", "pattern_area.R"))
source(here("Rscripts", "pattern_plot.R"))
source(here("Rscripts", "pattern_export.R"))
source(here("Rscripts", "pattern_animation.R"))
source(here("Rscripts", "pattern_io.R"))

#' Ler CSV e exibir como tabela formatada
#'
#' @param filepath Caminho do arquivo CSV
#' @param caption Título da tabela
#' @param col_names Nomes das colunas (NULL = usa cabeçalho do CSV)
#' @param font_size Tamanho da fonte (ex: 9)
#' @return Tabela kable formatada
read_and_display_csv <- function(filepath, 
                                 caption = NULL, 
                                 col_names = NULL,
                                 font_size = 9) {
  
  # Normalizar caminho
  filepath <- normalizePath(filepath, mustWork = TRUE)
  
  # Ler o arquivo
  dados <- readr::read_csv(filepath, show_col_types = FALSE)
  
  # Renomear colunas se fornecido
  if (!is.null(col_names)) {
    names(dados) <- col_names
  }
  
  # Criar tabela
  dados %>%
    knitr::kable(caption = caption, booktabs = TRUE, align = "c") %>%
    kableExtra::kable_styling(
      latex_options = c("scale_down", "hold_position", "striped"), 
      #full_width = TRUE,
      font_size = font_size
    )
}