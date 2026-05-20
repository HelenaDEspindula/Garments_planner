#' =============================================================================
#' Pattern Measurements — Leitura e Validação de Medidas
#' =============================================================================
#' Formato esperado dos CSVs:
#' 
#' Medidas (SeamlyMe):
#'   code,reference,description,value,formula
#' 
#' Ease:
#'   parameter,value,unit,description
#' =============================================================================

#' Ler arquivo de medidas no formato SeamlyMe
read_measurements <- function(filepath) {
  first_line <- readLines(filepath, n = 1)
  has_header <- grepl("^code,reference,description,value,formula", first_line)
  
  if (has_header) {
    raw <- readr::read_csv(filepath, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  } else {
    raw <- readr::read_csv(filepath, 
                           col_names = c("code", "reference", "description", "value", "formula"),
                           col_types = readr::cols(.default = "c"), 
                           show_col_types = FALSE)
  }
  
  measurements <- as.list(as.numeric(raw$value))
  names(measurements) <- raw$code
  
  na_codes <- names(measurements)[is.na(measurements)]
  if (length(na_codes) > 0) {
    warning("NA values in measurements: ", paste(na_codes, collapse = ", "))
  }
  
  measurements[!is.na(measurements)]
}

#' Ler arquivo de parâmetros de ease
read_ease <- function(filepath) {
  raw <- readr::read_csv(filepath, show_col_types = FALSE)
  params <- as.list(raw$value)
  names(params) <- raw$parameter
  params
}

#' Obter medida com validação
m_get <- function(measurements, code, required = TRUE) {
  value <- measurements[[code]]
  if (is.null(value)) {
    if (required) stop("Required measurement '", code, "' not found")
    return(0)
  }
  as.numeric(value)
}

#' Obter parâmetro de ease com valor padrão
e_get <- function(ease, param, default = 0) {
  value <- ease[[param]]
  if (is.null(value)) {
    warning("Ease parameter '", param, "' not found, using default: ", default)
    return(default)
  }
  as.numeric(value)
}

#' Validar medidas obrigatórias para um bloco
validate_measurements <- function(measurements, required) {
  missing <- setdiff(required, names(measurements))
  if (length(missing) > 0) {
    stop("Missing measurements: ", paste(missing, collapse = ", "))
  }
  
  zeros <- required[sapply(measurements[required], function(x) x == 0)]
  if (length(zeros) > 0) {
    warning("Zero values in required measurements: ", paste(zeros, collapse = ", "))
  }
  
  invisible(TRUE)
}

#' Criar template de medidas para preenchimento manual
create_measurement_template <- function(filepath, codes = NULL) {
  if (is.null(codes)) {
    codes <- c("height", "bust_circ", "waist_circ", "hip_circ",
               "neck_front_to_waist_f", "neck_back_to_waist_b",
               "shoulder_length", "armscye_circ", "neck_width")
  }
  
  template <- data.frame(
    code = codes,
    reference = paste0("XX", seq_along(codes)),
    description = codes,
    value = 0,
    formula = 0
  )
  
  write.csv(template, filepath, row.names = FALSE)
  message("Template created: ", filepath)
}