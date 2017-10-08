#' Report partial eta-squared given sums of squares
#'
#' @param model GLM object from which to extract SS and SSE
#' @param effect Parameter of interest
#' @template imports
#'
#' @export

partial_etasq <- function(model, effect, type = 3) {
  # Get relevant numbers from ANOVA table (SS, SSE, dfs)
  output <- tidy(Anova(model, type = type))
  SS <- subset(output, term == effect)[, "sumsq"]
  df.1 <- subset(output, term == effect)[, "df"]
  SSE <- subset(output, term == "Residuals")[, "sumsq"]
  df.2 <- subset(output, term == "Residuals")[, "df"]

  # Calculate partial eta squared
  partial.eta <- SS/(SS + SSE)
  partial.eta <- numformat(partial.eta)

  # Calclulate confidence limits
  Lims <- MBESS::conf.limits.ncf(F.value = (SS/df.1)/(SSE/df.2),
                          df.1 <- df.1, df.2 <- df.2,
                          conf.level = .95)
  Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  if(is.na(Lower.lim)) Lower.lim <- 0 # kludge to zero if NA
  Lower.lim <- numformat(Lower.lim)

  Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
  Upper.lim <- numformat(Upper.lim)

  # Print results
  paste0("partialetasq = ", partial.eta, ", [", Lower.lim, ", ", Upper.lim, "]")
}
