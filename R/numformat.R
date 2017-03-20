#' Format a number for APA style
#'
#' @param val The number to be formatted

numformat <- function(val) {
  sub("^(-?)0.", "\\1.", sprintf("%.2f", val))
}
