#' Fix a p-value to APA style
#'
#' This function drops the leading 0 and truncates low values to "p < .001"
#' @param p The p-value to be printed
#' @template imports
#'
#' @export

fix_p <- function(p) {
  p <- ifelse(p < .001,
              # If p < .001, make it "p < .001" and call it a day
              "*p* < .001",
              # Otherwise, round to three digits and lop off the leading decimal
              p %>%
                round(3) %>%
                as.character() %>%
                substring(2) %>%
                paste0("*p* = ", .))
  return(p)
}
