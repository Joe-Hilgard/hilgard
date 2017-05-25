#' Report partial eta-squared given sums of squares
#'
#' @param SS Effect's associated sum of squares.
#' @param SSE Model's sum of squared errors.
#' @param df.1 Degrees of freedom of the effect.
#' @param df.2 Degrees of freedom of the error term.
#' @param conf.level Confidence level of CI. Defaults to .95.
#' @param digits Significant digits to print in output. Defaults to 2.
#' @template imports

partial_etasq <- function(SS, SSE, df.1, df.2, conf.level = .95, digits = 2) {
  partial_eta <- SS/(SS + SSE)
  Lims <- conf.limits.ncf(F.value = (SS/df.1)/(SSE/df.2),
                          df.1 <- df.1, df.2 <- df.2,
                          conf.level = conf.level)
  Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  if(is.na(Lower.lim)) Lower.lim <- 0
  Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
  out <- list("etasq" = partial_eta, "CI" = c(Lower.lim, Upper.lim))
  out <- lapply(out, round, digits = digits)
  return(out)
}
