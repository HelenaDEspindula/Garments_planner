# build_bookdown_yml.R
# Gera _bookdown.yml automaticamente a partir de receitas/
# - Cada subpasta vira um capitulo
# - Pastas sem receitas sao ignoradas
# - Receitas entram em ordem alfabetica
# - Nao usa knit_child()

root <- "receitas"

if (!dir.exists(root)) {
  stop("Pasta 'receitas/' nao existe no diretorio atual.")
}

cats <- list.dirs(root, recursive = FALSE, full.names = FALSE)

# remove pastas ocultas E pastas tecnicas (_chapters, etc.)
cats <- cats[!grepl("^[\\._]", cats)]

# ordena alfabeticamente
cats <- cats[order(tolower(cats))]



# Helper: nome bonito do capitulo
pretty_title <- function(x) {
  x <- gsub("_", " ", x)
  words <- strsplit(x, " ")[[1]]
  paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words))) |>
    paste(collapse = " ")
}

rmd_files <- c("index.Rmd")

for (cat in cats) {
  cat_path <- file.path(root, cat)

  recipes <- list.files(
    cat_path,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )

  # remove possivel arquivo de capitulo da contagem
  recipes <- recipes[!grepl("^(00_|cat_)", basename(recipes))]


  # pula categorias vazias
  if (length(recipes) == 0) next

  # garante arquivo de capitulo
  chap_file <- file.path(cat_path, paste0("00_", cat, ".Rmd"))

  if (!file.exists(chap_file)) {
    writeLines(
      paste0("# ", pretty_title(cat)),
      chap_file
    )
  }

  # adiciona capitulo
  rmd_files <- c(rmd_files, chap_file)

  # adiciona receitas em ordem alfabetica
  recipes <- recipes[ order(tolower(basename(recipes))) ]
  rmd_files <- c(rmd_files, recipes)
}

if (length(rmd_files) == 1) {
  stop("Nenhuma receita .Rmd encontrada em subpastas de 'receitas/'.")
}

# normaliza caminhos (Windows-safe)
rmd_files <- gsub("\\\\", "/", rmd_files)

# escreve _bookdown.yml
writeLines(
  c(
    'book_filename: "caderno-receitas"',
    "rmd_files:",
    paste0("  - ", rmd_files)
  ),
  "_bookdown.yml"
)

cat("OK: _bookdown.yml gerado com", length(rmd_files) - 1, "arquivos.\n")

if (interactive()) {
  bookdown::render_book("index.Rmd", "bookdown::pdf_book")
}
