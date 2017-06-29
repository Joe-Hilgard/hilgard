#' Fix a p-value to APA style
#'
#' This function drops the leading 0 and truncates low values to "p < .001"
#' @param p The p-value to be printed
#' @template imports
#'
#' @export

fix_p <- function(p) {

  stopifnot(p <= 1 & p >= 0)

  if(p < .001){
    output <- "*p* < .001"
  } else {
    p <- round(p, 3)
    p <- as.character(p)

    # TODO: Implement catch for trailing zeroes if p = .100 or .010 or 1.000, etc
    if (p != 1) {
      p <- substring(p, 2)
    }
    output <- paste0("*p* = ", p)
  }

  return(output)
}
