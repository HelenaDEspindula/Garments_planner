#' =============================================================================
#' Pattern IO — Leitura e Escrita de Blocos (.rds)
#' =============================================================================
#' O arquivo .rds é o formato central do sistema.
#' Todo bloco gerado ou modificado é salvo como .rds.
#' Gráficos e exportações leem do .rds.
#' =============================================================================

#' Salvar bloco em arquivo .rds
#'
#' @param block Saída de draft_*() ou modify_*()
#' @param name Nome base do bloco (ex: "blusa_sj_size48")
#' @param subdir Subdiretório dentro de output/ (ex: "blocks")
#' @return Caminho completo do arquivo salvo
save_block <- function(block, name, subdir = "blocks") {
  dir.create(here("output", subdir), recursive = TRUE, showWarnings = FALSE)
  filepath <- here("output", subdir, paste0(name, ".rds"))
  saveRDS(block, filepath)
  message("Block saved: ", filepath)
  return(filepath)
}

#' Carregar bloco de arquivo .rds
#'
#' @param name Nome base do bloco (ex: "blusa_sj_size48")
#' @param subdir Subdiretório dentro de output/ (ex: "blocks")
#' @return Bloco carregado
load_block <- function(name, subdir = "blocks") {
  filepath <- here("output", subdir, paste0(name, ".rds"))
  if (!file.exists(filepath)) {
    stop("Block file not found: ", filepath)
  }
  block <- readRDS(filepath)
  message("Block loaded: ", filepath)
  return(block)
}

#' Listar blocos salvos
#'
#' @param subdir Subdiretório dentro de output/
#' @return Vetor com nomes dos blocos (sem extensão)
list_blocks <- function(subdir = "blocks") {
  dir_path <- here("output", subdir)
  if (!dir.exists(dir_path)) return(character(0))
  files <- list.files(dir_path, pattern = "\\.rds$")
  gsub("\\.rds$", "", files)
}

#' Informações resumidas do bloco
#'
#' @param block Bloco carregado
#' @return Lista com informações resumidas
block_info <- function(block) {
  if (!is.null(block$dimensions)) {
    # Bloco completo (frente + costas)
    list(
      type = "complete",
      pieces = c("front", "back"),
      dimensions = block$dimensions,
      n_points = length(block$points),
      n_curves = length(block$curves),
      area_total_m2 = block$area$total_area_m2,
      measurements = block$measurements
    )
  } else if (!is.null(block$piece)) {
    # Peça individual
    list(
      type = "piece",
      piece = block$piece,
      n_points = length(block$points),
      n_curves = length(block$curves),
      n_seams = length(block$seams),
      area_m2 = block$area$area_m2,
      metadata = block$metadata
    )
  } else {
    list(type = "unknown")
  }
}

#' Imprimir resumo do bloco
print_block_summary <- function(block) {
  info <- block_info(block)
  cat("\n========================================\n")
  cat("  BLOCK SUMMARY\n")
  cat("========================================\n")
  
  if (info$type == "complete") {
    cat(sprintf("  Type: Complete (%s)\n", paste(info$pieces, collapse = " + ")))
    cat(sprintf("  Width: %.1f cm | Height: %.1f cm\n", 
                info$dimensions$dim_width, info$dimensions$dim_height))
    cat(sprintf("  Points: %d | Curves: %d\n", info$n_points, info$n_curves))
    cat(sprintf("  Total area: %.2f m2\n", info$area_total_m2))
  } else if (info$type == "piece") {
    cat(sprintf("  Type: Piece (%s)\n", info$piece))
    cat(sprintf("  Points: %d | Curves: %d | Seams: %d\n", 
                info$n_points, info$n_curves, info$n_seams))
    cat(sprintf("  Area: %.2f m2\n", info$area_m2))
  }
  cat("========================================\n")
}