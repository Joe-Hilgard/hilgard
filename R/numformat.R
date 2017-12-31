#' Format a number bounded on -1, 1 for APA style
#'
#' Common targets for this function are p-values, correlations, R^2, etc.
#' @param val The number to be formatted
#'
#' @export

numformat <- function(val) {
  sub("^(-?)0.", "\\1.", sprintf("%.2f", val))
}
