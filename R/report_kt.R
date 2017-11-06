#' Report the Kruskal test of medians
#'
#' Report the results of Kruskal-Wallis Rank Sum test as performed by kruskal.test()
#'
#' @param model Model object from which to extract chisq-value.
#' @template imports
#'
#' @export

report_kt <- function(model) {
  p <- model$p.value
  p <- fix_p(p)

  paste0("chi-square(", model$parameter, ") = ",
         round(model$statistic, 2), ", ",
         fix_p(model$p.value))
}
