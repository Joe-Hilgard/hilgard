#' Report a t-value from a regression model
#'
#' Reports t, p-value, and ESCI on r for a model term.
#' TODO: Figure out if this is bivariate or partial r.
#' TODO: Report degrees of freedom.
#' @param model Model object from which to extract t-value.
#' @param effect Parameter with t-value of interest

report_t <- function(model, effect) {
  frame <- tidy(model)
  t <- with(frame, statistic[term == effect]) %>%
    round(2)
  p <- with(frame, p.value[term == effect]) %>%
    fix_p
  r <- with(frame, estimate[term == effect]) %>%
    round(2) %>%
    numformat()
  ci <- confint(model) %>%
    tidy %>%
    filter(.rownames == effect) %>%
    select(-c(.rownames)) %>%
    round(2) %>%
    numformat()
  paste0("*t* = ", t, ", ", p, ", *r* = ", r, " [", ci[1], ", ", ci[2], "]") %>%
    return()
}
