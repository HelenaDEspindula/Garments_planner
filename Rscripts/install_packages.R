# =============================================================================
# Pattern Making Textbook - Package Installation Script (CORRIGIDO)
# =============================================================================
# Usage: source("Rscripts/install_packages.R")
#
# This script installs (if necessary) and loads all packages required
# for the pattern making textbook project.
# =============================================================================

# --- Configuração inicial ---
options(repos = c(CRAN = "https://cran.rstudio.com"))
options(install.packages.check.type = "both")

# --- Verificar biblioteca padrão ---
cat("\n>>> Verificando bibliotecas R...\n")
cat("  Bibliotecas atuais:\n")
for (lib in .libPaths()) {
  cat(sprintf("    - %s\n", lib))
}

# Garantir que M:/Rlibrary existe e está no path
personal_lib <- "M:/Rlibrary"
if (!dir.exists(personal_lib)) {
  cat(sprintf("  Criando biblioteca: %s\n", personal_lib))
  dir.create(personal_lib, recursive = TRUE)
}

# Adicionar ao .libPaths() se não existir
if (!(personal_lib %in% .libPaths())) {
  .libPaths(c(personal_lib, .libPaths()))
  cat(sprintf("  Adicionado %s ao .libPaths()\n", personal_lib))
}

# --- CRAN Packages (otimizado) ---
p_cran <- c(
  # Core documentation & rendering
  "knitr",           # R Markdown engine
  "bookdown",        # Book-length documents
  "rmarkdown",       # R Markdown
  "tinytex",         # LaTeX distribution management
  
  # Data manipulation & visualization (tidyverse substitui muitos pacotes)
  "tidyverse",       # Includes dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr
  
  # Table formatting
  "kableExtra",      # Enhanced tables for LaTeX/PDF output
  
  # File & path management
  "here",            # Project-relative paths
  "fs",              # File system operations
  
  # Hashing (for plot cache invalidation)
  "digest",          # Hash functions for cache keys
  
  # Grid & graphics (grid é base R, não precisa instalar)
  "gridExtra",       # Arrange multiple grid plots
  "ggrepel",         # Non-overlapping text labels in plots
  
  # Export formats
  "jsonlite",        # JSON export/import for interoperability
  
  # Google Drive integration (future)
  "googledrive",     # Upload patterns to Google Drive
  
  # Animation
  "gifski",          # GIF animation creation
  
  # 3D visualization (future)
  "rgl",             # 3D visualization
  "threejs",         # Interactive 3D in browser
  
  # Additional utilities
  "withr",           # Run code with modified settings
  
  # Development tools
  "devtools",        # Package development
  "usethis",         # Project setup helpers
  "remotes",         # Install GitHub packages
  
  # Pipe operator (se quiser explicitamente, mas tidyverse já inclui)
  "magrittr"         # For %>% pipe (tidyverse já tem, mas explícito não dói)
)

# Remove redundâncias (pacotes já incluídos no tidyverse)
p_cran <- setdiff(p_cran, c("dplyr", "ggplot2", "tidyr", "readr", 
                            "purrr", "stringr", "tibble"))

# --- GitHub Packages ---
p_github <- c(
  # Add any GitHub-only packages here
)

# --- Helper Functions Melhoradas ---

#' Install and load packages with error handling
#' 
#' @param pkgs Character vector of package names
#' @param method Installation method: "cran" or "github"
#' @return Invisible list with success and failed vectors
install_and_load <- function(pkgs, method = "cran") {
  
  success <- character(0)
  failed  <- character(0)
  
  for (pkg in pkgs) {
    cat(sprintf("\n[%s] Processing: %s\n", toupper(method), pkg))
    
    # Verificar se é pacote base (grid, stats, etc.)
    if (pkg %in% c("grid", "stats", "graphics", "utils", "methods", "datasets")) {
      cat(sprintf("  %s é um pacote base do R, carregando...\n", pkg))
      library(pkg, character.only = TRUE, quietly = TRUE)
      success <- c(success, pkg)
      next
    }
    
    # Try to load first
    loaded <- tryCatch({
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE)
      )
      TRUE
    }, error = function(e) FALSE)
    
    if (loaded) {
      cat(sprintf("  ✓ Já instalado e carregado\n"))
      success <- c(success, pkg)
      next
    }
    
    # Install if not loaded
    cat(sprintf("  Instalando...\n"))
    
    installed <- tryCatch({
      if (method == "cran") {
        install.packages(pkg, 
                         dependencies = TRUE, 
                         quiet = FALSE,
                         lib = personal_lib)  # Especificar biblioteca
      } else if (method == "github") {
        if (!requireNamespace("remotes", quietly = TRUE)) {
          install.packages("remotes", quiet = TRUE, lib = personal_lib)
        }
        remotes::install_github(pkg, upgrade = "never", quiet = FALSE, lib = personal_lib)
      }
      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ ERRO na instalação: %s\n", e$message))
      FALSE
    })
    
    if (!installed) {
      failed <- c(failed, pkg)
      next
    }
    
    # Try loading again
    loaded_after <- tryCatch({
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE)
      )
      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ Instalado mas falhou ao carregar: %s\n", e$message))
      FALSE
    })
    
    if (loaded_after) {
      cat(sprintf("  ✓ Instalado e carregado com sucesso\n"))
      success <- c(success, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }
  
  invisible(list(success = success, failed = failed))
}

# =============================================================================
# Execute Installation
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("  PATTERN MAKING TEXTBOOK - PACKAGE INSTALLATION\n")
cat("============================================================\n")
cat(sprintf("  Date: %s\n", Sys.Date()))
cat(sprintf("  R version: %s\n", R.version.string))
cat(sprintf("  Personal library: %s\n", personal_lib))
cat("============================================================\n\n")

# --- CRAN Packages ---
cat(">>> Installing CRAN packages...\n")
result_cran <- install_and_load(p_cran, method = "cran")

# --- GitHub Packages ---
if (length(p_github) > 0) {
  cat("\n>>> Installing GitHub packages...\n")
  result_github <- install_and_load(p_github, method = "github")
}

# =============================================================================
# LaTeX Setup (Melhorado)
# =============================================================================

cat("\n>>> Checking LaTeX installation...\n")

# Verificar se tinytex está instalado
tinytex_installed <- requireNamespace("tinytex", quietly = TRUE)

if (!tinytex_installed) {
  cat("  Instalando pacote tinytex...\n")
  install.packages("tinytex", lib = personal_lib)
  library(tinytex)
  tinytex_installed <- TRUE
} else {
  library(tinytex)
}

if (tinytex_installed) {
  latex_installed <- tryCatch({
    tinytex::is_tinytex()
  }, error = function(e) FALSE)
  
  if (!latex_installed) {
    cat("  TinyTeX não encontrado. Instalando...\n")
    cat("  NOTA: Isso pode levar vários minutos e ~500MB de espaço.\n")
    cat("  Para instalação manual: https://yihui.org/tinytex/\n")
    
    install_tinytex <- tryCatch({
      tinytex::install_tinytex()
      TRUE
    }, error = function(e) {
      cat(sprintf("  ✗ Falha ao instalar TinyTeX: %s\n", e$message))
      FALSE
    })
    
    if (install_tinytex) {
      cat("  ✓ TinyTeX instalado com sucesso\n")
    }
  } else {
    cat("  ✓ LaTeX (TinyTeX) já instalado\n")
  }
} else {
  cat("  ✗ Não foi possível instalar o pacote tinytex\n")
}

# =============================================================================
# Create Required Directories (com verificação do here)
# =============================================================================

cat("\n>>> Creating project directories...\n")

# Verificar se here está carregado
if (!requireNamespace("here", quietly = TRUE)) {
  cat("  Instalando pacote 'here'...\n")
  install.packages("here", lib = personal_lib)
}
library(here)

required_dirs <- c(
  "images/cache",
  "output",
  "manim",
  "data/measurements",
  "data/parameters"
)

for (dir in required_dirs) {
  dir_path <- here::here(dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("  Created: %s\n", dir))
  } else {
    cat(sprintf("  Exists: %s\n", dir))
  }
}

# =============================================================================
# Verify Required Data Files
# =============================================================================

cat("\n>>> Checking required data files...\n")

required_files <- c(
  "data/measurements/sophia_jobim_size48.csv",
  "data/parameters/sophia_jobim_ease.csv",
  "data/measurements/sophia_jobim_armscye_table.csv"
)

missing_files <- character(0)

for (file in required_files) {
  file_path <- here::here(file)
  if (file.exists(file_path)) {
    cat(sprintf("  ✓ Found: %s\n", file))
  } else {
    cat(sprintf("  ✗ MISSING: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

# =============================================================================
# Test Pipe Operator
# =============================================================================

cat("\n>>> Testing pipe operator...\n")
if (requireNamespace("magrittr", quietly = TRUE)) {
  library(magrittr)
  test_result <- tryCatch({
    "OK" %>% print() %>% invisible()
    "  ✓ magrittr pipe (%>%) funcionando\n"
  }, error = function(e) {
    "  ✗ magrittr pipe não funcionou\n"
  })
  cat(test_result)
} else {
  cat("  ⚠ magrittr não disponível, usando pipe nativo (|>)\n")
}

# =============================================================================
# Final Report
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("  INSTALLATION REPORT\n")
cat("============================================================\n")

# CRAN results
cat(sprintf("\nCRAN packages:\n"))
cat(sprintf("  Success: %d\n", length(result_cran$success)))
cat(sprintf("  Failed:  %d\n", length(result_cran$failed)))

if (length(result_cran$failed) > 0) {
  cat("  Failed packages:\n")
  for (pkg in result_cran$failed) {
    cat(sprintf("    - %s\n", pkg))
  }
  cat("\n  Sugestões para pacotes com falha:\n")
  cat("    1. Verifique sua conexão com a internet\n")
  cat("    2. Tente instalar manualmente: install.packages('nome_do_pacote')\n")
  cat("    3. Verifique se o pacote é compatível com R 4.5\n")
}

# GitHub results
if (exists("result_github")) {
  cat(sprintf("\nGitHub packages:\n"))
  cat(sprintf("  Success: %d\n", length(result_github$success)))
  cat(sprintf("  Failed:  %d\n", length(result_github$failed)))
  
  if (length(result_github$failed) > 0) {
    cat("  Failed packages:\n")
    for (pkg in result_github$failed) {
      cat(sprintf("    - %s\n", pkg))
    }
  }
}

# Missing data files
if (length(missing_files) > 0) {
  cat("\n⚠ AVISO: Arquivos de dados ausentes:\n")
  for (file in missing_files) {
    cat(sprintf("    - %s\n", file))
  }
  cat("  O livro pode não compilar corretamente sem estes arquivos.\n")
}

# Overall status
all_failed <- result_cran$failed
if (exists("result_github")) {
  all_failed <- c(all_failed, result_github$failed)
}

if (length(all_failed) > 0) {
  cat("\n⚠ ATENÇÃO: Alguns pacotes falharam na instalação.\n")
  cat("  Verifique as mensagens de erro acima.\n")
  cat("  Você pode precisar instalar manualmente.\n")
} else {
  cat("\n✓ SUCESSO: Todos os pacotes foram instalados e carregados!\n")
}

cat("\n============================================================\n")
cat("  PRÓXIMOS PASSOS:\n")
cat("  1. Reinicie a sessão R: Session > Restart R (Ctrl+Shift+F10)\n")
cat("  2. Abra index.Rmd\n")
cat("  3. Execute: bookdown::render_book('index.Rmd', 'bookdown::pdf_book')\n")
cat("  4. O PDF estará na pasta 'output/'\n")
cat("============================================================\n")

# --- Session Info (for debugging) ---
cat("\n>>> Informação da sessão (para troubleshooting):\n")
print(sessionInfo())
cat(sprintf("\n>>> Biblioteca padrão: %s\n", personal_lib))
cat(sprintf(">>> Bibliotecas disponíveis: %s\n", paste(.libPaths(), collapse = ", ")))