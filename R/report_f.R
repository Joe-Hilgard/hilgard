#' Report an F-test
#'
#' Report an F-test as F(df1, df2) = F, p = .###.
#' @param model Model object from which to extract F-value.
#' @param effect Parameter with F-value of interest
#' @template imports
#'
#' @export

report_f <- function(model, effect) {

  frame <- tidy(car::Anova(model, type = 3))
  df1 <- with(frame, df[term == effect])
  df2 <- with(frame, df[term == "Residuals"])

  f <- with(frame, statistic[term == effect])
  f <- round(f, 2)

  p <- with(frame, p.value[term == effect])
  p <- fix_p(p)

  output <- paste0("*F*(", df1, ", ", df2, ") = ", f, ", ", p)
  return(output)
}
