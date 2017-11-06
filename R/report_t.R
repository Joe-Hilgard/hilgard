#' Report a t-value from a regression model
#'
#' Reports t, p-value, and ESCI on b for a model term.
#' TODO: Figure out if it's possible to get r out of this
#' TODO: consider implementing d for dichotomous predictors
#' @param model Model object from which to extract t-value.
#' @param effect Parameter with t-value of interest
#' @template imports
#'
#' @export

report_t <- function(model, effect) {
  frame <- broom::tidy(model)

  t <- with(frame, statistic[term == effect])
  t <- round(t, 2)

  df <- model$df.residual

  p <- with(frame, p.value[term == effect])
  p <- fix_p(p)

  b <- with(frame, estimate[term == effect])
  b <- numformat(b)

  ci <- confint(model)
  ci <- tidy(ci)
  ci <- filter(ci, .rownames == effect)
  ci <- select(ci, -c(.rownames))
  ci <- numformat(ci)
  # make t(df) = t, p = p
  t.out <- paste0("*t*(", df, ") = ", t, ", ", p)
  # make b = b [b.ll, b.ul]
  b.out <- paste0("*b* = ", b, " [", ci[1], ", ", ci[2], "]")

  output <- paste0(t.out, ", ", b.out)
  return(output)
}
