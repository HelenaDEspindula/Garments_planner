# =============================================================================
# Pattern Making Textbook - Package Installation Script
# =============================================================================
# Usage: source("Rscripts/install_packages.R")
# 
# This script installs (if necessary) and loads all packages required
# for the pattern making textbook project.
# =============================================================================

# --- CRAN Packages ---
p_cran <- c(
  # Core documentation & rendering
  "knitr",           # R Markdown engine
  "bookdown",        # Book-length documents
  "rmarkdown",       # R Markdown
  "tinytex",         # LaTeX distribution management
  
  # Data manipulation & visualization
  "tidyverse",       # Includes dplyr, ggplot2, tidyr, readr, purrr, tibble
  "ggplot2",         # Plotting
  "dplyr",           # Data manipulation
  "tidyr",           # Data tidying
  "readr",           # Reading CSV files
  "purrr",           # Functional programming
  
  # Table formatting
  "kableExtra",      # Enhanced tables for LaTeX/PDF output
  
  # File & path management
  "here",            # Project-relative paths
  "fs",              # File system operations
  
  # String & text processing
  "stringr",         # String manipulation
  
  # Hashing (for plot cache invalidation)
  "digest",          # Hash functions for cache keys
  
  # Grid & graphics
  "grid",            # Low-level graphics (base R)
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
  "remotes"          # Install GitHub packages
)

# --- GitHub Packages (if any) ---
p_github <- c(
  # Add any GitHub-only packages here
)

# --- Helper Functions ---

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
    
    # Try to load first
    loaded <- tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    if (loaded) {
      cat(sprintf("  Already installed and loaded\n"))
      success <- c(success, pkg)
      next
    }
    
    # Install if not loaded
    cat(sprintf("  Installing...\n"))
    
    installed <- tryCatch({
      if (method == "cran") {
        install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      } else if (method == "github") {
        if (!requireNamespace("remotes", quietly = TRUE)) {
          install.packages("remotes", quiet = TRUE)
        }
        remotes::install_github(pkg, upgrade = "never", quiet = FALSE)
      }
      TRUE
    }, error = function(e) {
      cat(sprintf("  ERROR: %s\n", e$message))
      FALSE
    })
    
    if (!installed) {
      failed <- c(failed, pkg)
      next
    }
    
    # Try loading again
    loaded_after <- tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
      TRUE
    }, error = function(e) {
      cat(sprintf("  Installed but failed to load: %s\n", e$message))
      FALSE
    })
    
    if (loaded_after) {
      cat(sprintf("  Installed and loaded successfully\n"))
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
# LaTeX Setup (if needed)
# =============================================================================

cat("\n>>> Checking LaTeX installation...\n")

latex_installed <- tryCatch({
  tinytex::is_tinytex()
}, error = function(e) FALSE)

if (!latex_installed) {
  cat("  TinyTeX not found. Installing...\n")
  cat("  NOTE: This may take several minutes and ~500MB of disk space.\n")
  cat("  If installation fails, install manually: https://yihui.org/tinytex/\n")
  
  install_tinytex <- tryCatch({
    tinytex::install_tinytex()
    TRUE
  }, error = function(e) {
    cat(sprintf("  Failed to install TinyTeX: %s\n", e$message))
    FALSE
  })
  
  if (install_tinytex) {
    cat("  TinyTeX installed successfully\n")
  }
} else {
  cat("  LaTeX (TinyTeX) already installed\n")
}

# =============================================================================
# Create Required Directories
# =============================================================================

cat("\n>>> Creating project directories...\n")

required_dirs <- c(
  "images/cache",
  "output",
  "manim",
  "data/measurements",
  "data/parameters"
)

for (dir in required_dirs) {
  dir_path <- here(dir)
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

for (file in required_files) {
  file_path <- here(file)
  if (file.exists(file_path)) {
    cat(sprintf("  Found: %s\n", file))
  } else {
    cat(sprintf("  MISSING: %s\n", file))
  }
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

# Overall status
all_failed <- c(result_cran$failed)
if (exists("result_github")) {
  all_failed <- c(all_failed, result_github$failed)
}

if (length(all_failed) > 0) {
  cat("\n*** WARNING: Some packages failed to install. ***\n")
  cat("  Check the error messages above.\n")
  cat("  You may need to install them manually.\n")
  cat("  Common issues:\n")
  cat("    - R version too old (update R first)\n")
  cat("    - Missing system dependencies (check package documentation)\n")
  cat("    - Network issues (try again later)\n")
} else {
  cat("\n*** SUCCESS: All packages installed and loaded! ***\n")
}

cat("\n============================================================\n")
cat("  NEXT STEPS:\n")
cat("  1. Restart R session: Session > Restart R (Ctrl+Shift+F10)\n")
cat("  2. Open index.Rmd\n")
cat("  3. Run: bookdown::render_book('index.Rmd', 'bookdown::pdf_book')\n")
cat("  4. Find the PDF in the 'output/' folder\n")
cat("============================================================\n")

# --- Session Info (for debugging) ---
cat("\n>>> Session information (for troubleshooting):\n")
sessionInfo()