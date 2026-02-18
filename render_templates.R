## source("render_templates.R")

library(rmarkdown)
library(bookdown)
library(withr)


message("\n=== POPs (PDF) ===")
pops <- c(
  # ,
  "01_modelo_projeto.Rmd",
  "02_ficha_de_moldes.Rmd",
  "03_fabric_stach.Rmd"
  # ...adicione os demais
)

#dir.create("garments_templates/_pdf", recursive = TRUE, showWarnings = FALSE)

with_dir("garments_templates", {
  for (f in pops) {
    message(f)
    rmarkdown::render(f, output_dir = "_pdf", quiet = TRUE)
  }
})




