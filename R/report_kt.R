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
  p <- fix_p(p)

  paste0("chi-square(", model$parameter, ") = ", round(model$statistic, 2), ", ", p)
}
