#' Report a t-value from a regression model
#'
#' Reports t, p-value, and ESCI on b for a model term.
#' @param model Model object from which to extract t-value.
#' @param effect Parameter with t-value of interest
#'
#' @export

report_t <- function(model, effect, metric = "b", n1, n2) {
  frame <- broom::tidy(model)

  t <- with(frame, statistic[term == effect])
  t <- round(t, 2)

  df <- model$df.residual

  p <- with(frame, p.value[term == effect])
  p <- fix_p(p)

  if(metric == "b") {
    b <- with(frame, estimate[term == effect])
    b <- numformat(b)

    ci <- confint(model)
    ci <- broom::tidy(ci)
    ci <- dplyr::filter(ci, .rownames == effect)
    ci <- dplyr::select(ci, -c(.rownames))
    ci <- numformat(ci)

    # make b = b [b.ll, b.ul]
    esci.out <- paste0("*b* = ", b, " [", ci[1], ", ", ci[2], "]")
  }

  if (metric == "d") {
    # if(n1 == NULL | n2 == NULL) {
    #   n1 <- ???
    #   n2 <- ???
    # }
    esci <- with(frame, statistic[term == effect]) %>%
      compute.es::tes(n1, n2, verbose = F)

    esci.out <- paste0("*d* = ", esci$d, " [", esci$l.d, ", ", esci$u.d, "]")
  }

  if (metric == "r") {
    esci <- with(frame, statistic[term == effect]) %>%
      compute.es::tes(n1, n2, verbose = F)

    # If t is positive, r is positive
    if (with(frame, statistic[term == effect]) >= 0) {
      esci.out <- paste0("*r* = ", numformat(esci$r),
                         " [", numformat(esci$l.r), ", ", numformat(esci$u.r), "]")
    }

    # If t is negative, r is negative
    if (with(frame, statistic[term == effect]) < 0) {
      esci.out <- paste0("*r* = ", numformat(-esci$r),
                         " [", numformat(-esci$u.r), ", ", numformat(-esci$l.r), "]")
    }
  }

  # make t(df) = t, p = p
  t.out <- paste0("*t*(", df, ") = ", t, ", ", p)

  output <- paste0(t.out, ", ", esci.out)
  return(output)
}
