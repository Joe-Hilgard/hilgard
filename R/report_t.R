#' Report a t-value from a regression model
#'
#' Reports t, p-value, and ESCI on b for a model term.
#' TODO: Figure out if it's possible to get r out of this
#' @param model Model object from which to extract t-value.
#' @param effect Parameter with t-value of interest
#' @template imports
#'
#' @export

report_t <- function(model, effect) {
  frame <- tidy(model)
  t <- with(frame, statistic[term == effect]) %>%
    round(2)
  df <- model$df.residual
  p <- with(frame, p.value[term == effect]) %>%
    fix_p
  b <- with(frame, estimate[term == effect]) %>%
    round(2) %>%
    numformat()
  ci <- confint(model) %>%
    tidy %>%
    filter(.rownames == effect) %>%
    select(-c(.rownames)) %>%
    round(2) %>%
    numformat()
  # make t(df) = t, p = p
  t.out <- paste0("*t*(", df, ") = ", t, ", ", p)
  # make b = b [b.ll, b.ul]
  b.out <- paste0("*b* = ", b, " [", ci[1], ", ", ci[2], "]")
  paste0(t.out, ", ", b.out)  %>%
    return()
}
