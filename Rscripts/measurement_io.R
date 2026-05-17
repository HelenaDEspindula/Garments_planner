#' Measurement I/O functions
#'
#' Handles reading SeamlyMe CSV exports and ease parameters

#' Read SeamlyMe measurements CSV
#' @param filepath Path to CSV file
#' @return Named list of measurement values in cm
read_measurements <- function(filepath) {
  raw <- readr::read_csv(
    filepath,
    col_names = c("code", "reference", "description", "value", "formula"),
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
  
  # Convert to named list
  measurements <- as.list(as.numeric(raw$value))
  names(measurements) <- raw$code
  
  # Remove NAs and warn
  na_codes <- names(measurements)[is.na(measurements)]
  if (length(na_codes) > 0) {
    warning("NA values in measurements: ", paste(na_codes, collapse = ", "))
  }
  
  measurements[!is.na(measurements)]
}

#' Read ease parameters CSV
#' @param filepath Path to CSV file
#' @return Named list of parameter values
read_ease <- function(filepath) {
  raw <- readr::read_csv(filepath, show_col_types = FALSE)
  
  params <- as.list(raw$value)
  names(params) <- raw$parameter
  
  params
}

#' Get measurement with validation
#' @param measurements Named list of measurements
#' @param code Measurement code
#' @param required If TRUE, error on missing
#' @return Measurement value
m_get <- function(measurements, code, required = TRUE) {
  value <- measurements[[code]]
  if (is.null(value)) {
    if (required) {
      stop("Required measurement '", code, "' not found")
    }
    return(0)
  }
  as.numeric(value)
}

#' Get ease parameter with default
#' @param ease Named list of ease parameters
#' @param param Parameter name
#' @param default Default value if not found
#' @return Parameter value
e_get <- function(ease, param, default = 0) {
  value <- ease[[param]]
  if (is.null(value)) {
    warning("Ease parameter '", param, "' not found, using default: ", default)
    return(default)
  }
  as.numeric(value)
}

#' Validate measurements for a specific block
#' @param measurements Named list of measurements
#' @param required Vector of required measurement codes
#' @return Logical TRUE if all required present
validate_measurements <- function(measurements, required) {
  missing <- setdiff(required, names(measurements))
  
  if (length(missing) > 0) {
    stop("Missing measurements: ", paste(missing, collapse = ", "))
  }
  
  # Check for zero values
  zeros <- required[sapply(measurements[required], function(x) x == 0)]
  if (length(zeros) > 0) {
    warning("Zero values in required measurements: ", paste(zeros, collapse = ", "))
  }
  
  invisible(TRUE)
}

#' Create a template measurements file for manual editing
#' @param filepath Output file path
#' @param codes Vector of measurement codes to include
create_measurement_template <- function(filepath, codes = NULL) {
  if (is.null(codes)) {
    codes <- c("height", "bust_circ", "waist_circ", "hip_circ",
               "neck_front_to_waist_f", "back_waist_length",
               "shoulder_length", "armscye_circ", "neck_width",
               "bustpoint_to_bustpoint", "across_chest_f")
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