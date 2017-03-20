#' Report a z-test from a logistic regression model
#'
#' Report a z-test as z = z, p = .###, OR = OR [OR.LL, OR.UL].
#' @param model Model object from which to extract F-value.
#' @param effect Parameter with F-value of interest

report_z <- function(model, effect) {
  frame <- tidy(model)
  z <- with(frame, statistic[term == effect]) %>%
    round(2)
  p <- with(frame, p.value[term == effect]) %>%
    fix_p
  OR <- with(frame, estimate[term == effect]) %>%
    exp %>%
    round(2)
  ci <- confint(model) %>%
    tidy() %>%
    filter(.rownames == effect) %>%
    select(-.rownames) %>%
    exp() %>%
    round(2)
  paste0("*z* = ", z, ", ", p, ", *OR* = ", OR, " [", ci[1], ", ", ci[2], "]") %>%
    return()
}
