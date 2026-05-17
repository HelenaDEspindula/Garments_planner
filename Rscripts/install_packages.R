# =============================================================================
# Pattern Making Textbook - Package Installation Script
# =============================================================================
# Usage: source("install_packages.R")
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
  "ggplot2",         # Plotting (explicit, even though in tidyverse)
  "dplyr",           # Data manipulation (explicit)
  "tidyr",           # Data tidying (explicit)
  "readr",           # Reading CSV files (explicit)
  "purrr",           # Functional programming (explicit)
  
  # Table formatting
  "kableExtra",      # Enhanced tables for LaTeX/PDF output
  
  # File & path management
  "here",            # Project-relative paths
  "fs",              # File system operations
  
  # String & text processing
  "stringr",         # String manipulation (in tidyverse, explicit for clarity)
  
  # Hashing (for plot cache invalidation)
  "digest",          # Hash functions for cache keys
  
  # Grid & graphics
  "grid",            # Low-level graphics (base R, explicit)
  "gridExtra",       # Arrange multiple grid plots
  
  # Additional utilities
  "withr",           # Run code with modified settings
  "jsonlite",        # JSON parsing (for potential future use)
  
  # Development tools
  "devtools",        # Package development
  "usethis"          # Project setup helpers
)

# --- GitHub Packages (if any) ---
# Currently none, but placeholder for future:
# p_github <- c(
#   "username/package_name"
# )

# --- Helper Functions ---

#' Install and load packages with error handling
#' 
#' @param pkgs Character vector of package names
#' @param method Installation method: "cran" or "github"
#' @return Invisible list with success and failed vectors
install_and_load <- function(pkgs, method = "cran") {
  
  success <- character(0)
  failed  <- character(0)
  skipped <- character(0)
  
  for (pkg in pkgs) {
    cat(sprintf("\n[%s] Processing: %s\n", toupper(method), pkg))
    
    # Try to load first
    loaded <- tryCatch({
      library(pkg, character.only = TRUE, quietly = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    if (loaded) {
      cat(sprintf("  ✓ Already installed and loaded\n"))
      success <- c(success, pkg)
      next
    }
    
    # Install if not loaded
    cat(sprintf("  ⟳ Installing...\n"))
    
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
      cat(sprintf("  ✗ ERROR: %s\n", e$message))
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
      cat(sprintf("  ✗ Installed but failed to load: %s\n", e$message))
      FALSE
    })
    
    if (loaded_after) {
      cat(sprintf("  ✓ Installed and loaded successfully\n"))
      success <- c(success, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }
  
  invisible(list(success = success, failed = failed, skipped = skipped))
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
# cat("\n>>> Installing GitHub packages...\n")
# result_github <- install_and_load(p_github, method = "github")

# =============================================================================
# LaTeX Setup (if needed)
# =============================================================================

cat("\n>>> Checking LaTeX installation...\n")

latex_installed <- tryCatch({
  tinytex::is_tinytex()
}, error = function(e) FALSE)

if (!latex_installed) {
  cat("  ⟳ TinyTeX not found. Installing...\n")
  cat("  NOTE: This may take several minutes and ~500MB of disk space.\n")
  
  install_tinytex <- tryCatch({
    tinytex::install_tinytex()
    TRUE
  }, error = function(e) {
    cat(sprintf("  ✗ Failed to install TinyTeX: %s\n", e$message))
    cat("  → You can install LaTeX manually: https://yihui.org/tinytex/\n")
    FALSE
  })
  
  if (install_tinytex) {
    cat("  ✓ TinyTeX installed successfully\n")
  }
} else {
  cat("  ✓ LaTeX (TinyTeX) already installed\n")
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

# Overall status
all_failed <- c(result_cran$failed)  # Add result_github$failed if used

if (length(all_failed) > 0) {
  cat("\n⚠ WARNING: Some packages failed to install.\n")
  cat("  Check the error messages above.\n")
  cat("  You may need to install them manually.\n")
  cat("  Common issues:\n")
  cat("    - R version too old (update R first)\n")
  cat("    - Missing system dependencies (check package documentation)\n")
  cat("    - Network issues (try again later)\n")
} else {
  cat("\n✓ SUCCESS: All packages installed and loaded!\n")
}

cat("\n============================================================\n")
cat("  NEXT STEPS:\n")
cat("  1. Open pattern-making-textbook.Rproj in RStudio\n")
cat("  2. Run: bookdown::render_book('index.Rmd', 'bookdown::pdf_book')\n")
cat("  3. Find the PDF in the 'output/' folder\n")
cat("============================================================\n")

# --- Session Info (for debugging) ---
cat("\n>>> Session information (for troubleshooting):\n")
sessionInfo()