#' Extract Numeric Value from Text
#'
#' Uses a regular expression to extract the first numeric value found in a text string.
#'
#' @param text A character string that contains numeric information.
#' @return A numeric value representing the first number found in the text, or NA if no numeric value is present.
#' @examples
#' # Example: Extract the numeric value from a string
#' value <- smndata_extract_numeric("Latitud: 19.4567")
#' print(value)  # Expected output: 19.4567
#' @export
smndata_extract_numeric <- function(text) {
  num <- stringr::str_extract(text, "[-+]?[0-9]*\\.?[0-9]+")
  as.numeric(num)
}
