#' Report the kt test
#'
#' I don't remember what this was for -- some kind of goodness-of-fit test?
#' TODO: Figure that out.
#' @param model Model object from which to extract chisq-value.
#' @template imports
#'
#' @export


report_kt <- function(model) {
  p <- model$p.value
  p <- ifelse(p < .001,
              # If p < .001, make it "p < .001" and call it a day
              "*p* < .001",
              # Otherwise, round to three digits and lop off the leading decimal
              p %>%
                round(3) %>%
                as.character() %>%
                substring(2) %>%
                paste0("*p* = ", .))
  paste0("chi-square(", model$parameter, ") = ", round(model$statistic, 2), ", ", p)
}
