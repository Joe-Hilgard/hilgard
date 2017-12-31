#' Report a likelihood-ratio chisq-test
#'
#' Appropriate for use with "glm" objects.
#' Report an LR chisq-test as chisq(df) = chisq, p = .###.
#' @param model Model object from which to extract chisq-value.
#' @param effect Parameter with chisq-value of interest
#'
#' @export

report_LRchisq <- function(model, effect) {

  frame <- broom::tidy(car::Anova(model, type = 3))
  df <- with(frame, df[term == effect])

  chisq <- with(frame, LR.Chisq[term == effect])
  chisq <- round(chisq, 2)

  p <- with(frame, p.value[term == effect])
  p <- fix_p(p)

  output <- paste0("*chisq*(", df, ") = ", chisq, ", ", p)
  return(output)
}
