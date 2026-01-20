## Renderiza todos os modelos do Garments Planner em PDFs individuais
##
## Uso (terminal):
##   Rscript render_garments_templates.R
##
## Uso (RStudio):
##   source("render_garments_templates.R")

suppressPackageStartupMessages({
  library(rmarkdown)
  library(withr)
})

library(rmarkdown)
library(bookdown)
library(withr)


message("\n=== Garments Templates (PDF) ===")
pops <- c(
  "00_medidas_do_corpo.Rmd",
  "01_modelo_projeto.Rmd",
  "02_checklist_blusa_simples.Rmd"
  # ...adicione os demais
)

dir.create("garments_templates/_pdf", recursive = TRUE, showWarnings = FALSE)

with_dir("garments_templates/", {
  for (f in pops) {
    message(f)
    rmarkdown::render(f, output_dir = "_pdf", quiet = TRUE)
  }
})

message("\nOK! PDFs gerados em: ", out_dir)
