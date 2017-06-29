#' Report a z-test from a logistic regression model
#'
#' Report a z-test as z = z, p = .###, OR = OR [OR.LL, OR.UL].
#' @param model Model object from which to extract F-value.
#' @param effect Parameter with F-value of interest
#' @template imports
#'
#' @export

report_z <- function(model, effect) {
  frame <- tidy(model)
  z <- with(frame, statistic[term == effect])
  z <- round(2)

  p <- with(frame, p.value[term == effect])
  p <- fix_p(p)

  OR <- with(frame, estimate[term == effect])
  OR <- exp(OR)
  OR <- round(OR, 2)

  ci <- confint(model)
  ci <- tidy(ci)
  ci <- filter(ci, .rownames == effect)
  ci <- select(ci, -.rownames)
  ci <- exp(ci)
  ci <- round(ci, 2)

  output <- paste0("*z* = ", z, ", ", p, ", *OR* = ", OR, " [", ci[1], ", ", ci[2], "]")
  return(output)
}
