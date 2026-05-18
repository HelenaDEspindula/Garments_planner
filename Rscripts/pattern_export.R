#' =============================================================================
#' Pattern Export — Exportação para Formatos Externos
#' =============================================================================
#' Formatos suportados:
#'   - SVG (para edição vetorial)
#'   - DXF (para CAD/Seamly2D)
#'   - Google Drive (upload automático)
#'   - JSON (para interoperabilidade)
#' =============================================================================

source(here("Rscripts", "pattern_core.R"))

#' Exportar peça para SVG
#'
#' @param block Saída de draft_*() ou peça individual
#' @param output_file Caminho do arquivo .svg
#' @param include_grid Incluir grid de referência?
#' @param include_labels Incluir labels dos pontos?
#' @return Caminho do arquivo gerado (invisível)
export_svg <- function(block, output_file, 
                       include_grid = TRUE, 
                       include_labels = FALSE) {
  
  # Determinar bounds
  if (!is.null(block$dimensions)) {
    # Bloco completo
    width <- block$dimensions$dim_width
    height <- block$dimensions$dim_height
    pieces <- list(block$front, block$back)
  } else {
    # Peça individual
    width <- block$metadata$dimensions$width
    height <- block$metadata$dimensions$height
    pieces <- list(block)
  }
  
  margin <- 5
  viewbox_width <- width + 2 * margin
  viewbox_height <- height + 2 * margin
  
  # Abrir arquivo SVG
  sink(output_file)
  
  cat(sprintf('<?xml version="1.0" encoding="UTF-8"?>\n'))
  cat(sprintf('<svg xmlns="http://www.w3.org/2000/svg" 
    viewBox="0 0 %.0f %.0f" width="%.0fmm" height="%.0fmm">\n',
              viewbox_width, viewbox_height, viewbox_width * 10, viewbox_height * 10))
  
  # Grid
  if (include_grid) {
    cat('<g stroke="#E0E0E0" stroke-width="0.2">\n')
    for (x in seq(0, width, by = 5)) {
      cat(sprintf('  <line x1="%.1f" y1="0" x2="%.1f" y2="%.0f"/>\n', 
                  x + margin, x + margin, height))
    }
    for (y in seq(0, height, by = 5)) {
      cat(sprintf('  <line x1="0" y1="%.1f" x2="%.0f" y2="%.1f"/>\n', 
                  y + margin, width, y + margin))
    }
    cat('</g>\n')
  }
  
  # Desenhar cada peça
  for (piece in pieces) {
    piece_color <- if (piece$piece == "front") "#2196F3" else "#FF9800"
    
    cat(sprintf('<g stroke="%s" stroke-width="1.5" fill="none">\n', piece_color))
    
    # Costuras (linhas retas)
    for (seam in piece$seams) {
      from_pt <- piece$points[[seam$from]]
      to_pt <- piece$points[[seam$to]]
      
      if (!is.null(from_pt) && !is.null(to_pt)) {
        cat(sprintf('  <line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>\n',
                    from_pt[1] + margin, height - from_pt[2] + margin,
                    to_pt[1] + margin, height - to_pt[2] + margin))
      }
    }
    
    # Curvas (polilinhas)
    for (curve_name in names(piece$curves)) {
      curve_data <- piece$curves[[curve_name]]
      cat(sprintf('  <polyline points="'))
      for (i in 1:nrow(curve_data)) {
        cat(sprintf('%.2f,%.2f ', 
                    curve_data$x[i] + margin, 
                    height - curve_data$y[i] + margin))
      }
      cat(sprintf('"/>\n'))
    }
    
    cat('</g>\n')
    
    # Labels
    if (include_labels) {
      cat(sprintf('<g fill="#333333" font-size="3" font-family="Arial">\n'))
      for (pt_name in names(piece$points)) {
        pt <- piece$points[[pt_name]]
        label <- gsub("point_", "", pt_name)
        cat(sprintf('  <text x="%.2f" y="%.2f">%s</text>\n',
                    pt[1] + margin + 0.5, 
                    height - pt[2] + margin - 0.5,
                    label))
      }
      cat('</g>\n')
    }
  }
  
  cat('</svg>\n')
  sink()
  
  message("SVG exported: ", output_file)
  invisible(output_file)
}

#' Exportar peça para DXF (compatível com Seamly2D)
#'
#' @param block Saída de draft_*()
#' @param output_file Caminho do arquivo .dxf
#' @return Caminho do arquivo gerado (invisível)
export_dxf <- function(block, output_file) {
  
  sink(output_file)
  
  # Header DXF
  cat("0\nSECTION\n")
  cat("2\nENTITIES\n")
  
  entity_count <- 0
  
  # Processar todas as peças
  pieces <- if (!is.null(block$dimensions)) {
    list(block$front, block$back)
  } else {
    list(block)
  }
  
  for (piece in pieces) {
    
    # Linhas
    for (seam in piece$seams) {
      from_pt <- piece$points[[seam$from]]
      to_pt <- piece$points[[seam$to]]
      
      if (!is.null(from_pt) && !is.null(to_pt)) {
        cat("0\nLINE\n")
        cat("8\n0\n")  # Layer 0
        cat(sprintf("10\n%.4f\n", from_pt[1] * 10))  # X1 (mm)
        cat(sprintf("20\n%.4f\n", from_pt[2] * 10))  # Y1 (mm)
        cat(sprintf("11\n%.4f\n", to_pt[1] * 10))    # X2 (mm)
        cat(sprintf("21\n%.4f\n", to_pt[2] * 10))    # Y2 (mm)
        entity_count <- entity_count + 1
      }
    }
    
    # Curvas como polilinhas
    for (curve_name in names(piece$curves)) {
      curve_data <- piece$curves[[curve_name]]
      
      if (nrow(curve_data) > 1) {
        cat("0\nLWPOLYLINE\n")
        cat("8\n0\n")
        cat("90\n")
        cat(sprintf("%d\n", nrow(curve_data)))
        cat("70\n0\n")
        
        for (i in 1:nrow(curve_data)) {
          cat(sprintf("10\n%.4f\n", curve_data$x[i] * 10))
          cat(sprintf("20\n%.4f\n", curve_data$y[i] * 10))
        }
        
        entity_count <- entity_count + 1
      }
    }
  }
  
  # Footer DXF
  cat("0\nENDSEC\n")
  cat("0\nEOF\n")
  
  sink()
  
  message("DXF exported: ", output_file, " (", entity_count, " entities)")
  invisible(output_file)
}

#' Upload de arquivo para Google Drive
#'
#' @param file_path Caminho do arquivo local
#' @param folder_id ID da pasta no Google Drive (NULL = raiz)
#' @param filename Nome do arquivo no Drive (NULL = mesmo nome)
#' @return ID do arquivo no Drive
upload_to_gdrive <- function(file_path, folder_id = NULL, filename = NULL) {
  
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is required. Install with: install.packages('googledrive')")
  }
  
  # Autenticar (se necessário)
  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }
  
  # Preparar metadados
  if (is.null(filename)) {
    filename <- basename(file_path)
  }
  
  media <- googledrive::drive_upload(
    media = file_path,
    name = filename,
    path = if (!is.null(folder_id)) googledrive::as_id(folder_id) else NULL
  )
  
  message("Uploaded to Google Drive: ", media$id)
  invisible(media$id)
}

#' Salvar bloco como JSON para interoperabilidade
#'
#' @param block Saída de draft_*()
#' @param output_file Caminho do arquivo .json
#' @return Caminho do arquivo gerado (invisível)
export_json <- function(block, output_file) {
  
  # Função auxiliar para converter pontos para lista simples
  points_to_list <- function(points) {
    lapply(points, function(pt) list(x = unname(pt[1]), y = unname(pt[2])))
  }
  
  # Função auxiliar para converter curvas
  curves_to_list <- function(curves) {
    lapply(curves, function(curve) {
      list(x = curve$x, y = curve$y)
    })
  }
  
  # Construir estrutura
  if (!is.null(block$dimensions)) {
    # Bloco completo
    json_data <- list(
      type = "blouse",
      method = "sophia_jobim",
      front = list(
        points = points_to_list(block$front$points),
        curves = curves_to_list(block$front$curves),
        seams = block$front$seams
      ),
      back = list(
        points = points_to_list(block$back$points),
        curves = curves_to_list(block$back$curves),
        seams = block$back$seams
      ),
      dimensions = block$dimensions,
      metadata = list(
        measurements = block$measurements,
        ease = block$ease
      )
    )
  } else {
    # Peça individual
    json_data <- list(
      type = block$piece,
      method = block$metadata$method,
      points = points_to_list(block$points),
      curves = curves_to_list(block$curves),
      seams = block$seams,
      metadata = block$metadata
    )
  }
  
  # Escrever JSON
  jsonlite::write_json(json_data, output_file, 
                       pretty = TRUE, 
                       auto_unbox = TRUE)
  
  message("JSON exported: ", output_file)
  invisible(output_file)
}

#' Importar bloco de JSON
#'
#' @param input_file Caminho do arquivo .json
#' @return Bloco reconstruído
import_json <- function(input_file) {
  
  json_data <- jsonlite::read_json(input_file, simplifyVector = FALSE)
  
  # Reconstruir pontos
  rebuild_points <- function(pts_list) {
    points <- list()
    for (name in names(pts_list)) {
      points[[name]] <- c(pts_list[[name]]$x, pts_list[[name]]$y)
    }
    points
  }
  
  # Reconstruir curvas
  rebuild_curves <- function(curves_list) {
    curves <- list()
    for (name in names(curves_list)) {
      curves[[name]] <- data.frame(
        x = curves_list[[name]]$x,
        y = curves_list[[name]]$y
      )
    }
    curves
  }
  
  if (json_data$type == "blouse") {
    list(
      front = list(
        points = rebuild_points(json_data$front$points),
        curves = rebuild_curves(json_data$front$curves),
        seams = json_data$front$seams
      ),
      back = list(
        points = rebuild_points(json_data$back$points),
        curves = rebuild_curves(json_data$back$curves),
        seams = json_data$back$seams
      ),
      dimensions = json_data$dimensions,
      measurements = json_data$metadata$measurements,
      ease = json_data$metadata$ease
    )
  } else {
    list(
      piece = json_data$type,
      points = rebuild_points(json_data$points),
      curves = rebuild_curves(json_data$curves),
      seams = json_data$seams,
      metadata = json_data$metadata
    )
  }
}