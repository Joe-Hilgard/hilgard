#' Report partial eta-squared given sums of squares
#'
#' @param model GLM object from which to extract SS and SSE
#' @param effect Parameter of interest
#' @template imports
#'
#' @export

partial_etasq <- function(model, effect, type = 3) {
  output <- tidy(Anova(model, type = type))
  SS <- subset(output, term == effect)[, "sumsq"]
  df.1 <- subset(output, term == effect)[, "df"]
  SSE <- subset(output, term == "Residuals")[, "sumsq"]
  df.2 <- subset(output, term == "Residuals")[, "df"]

  # Calculate partial eta squared
  partial.eta <- round(SS/(SS + SSE), 2)
  partial.eta <- substring(as.character(partial.eta), 2)
  if(partial.eta == 0) partial.eta <- ".00" #kludge for v small effect

  # Calclulate confidence limits
  Lims <- conf.limits.ncf(F.value = (SS/df.1)/(SSE/df.2),
                          df.1 <- df.1, df.2 <- df.2,
                          conf.level = .95)
  Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
  if(is.na(Lower.lim)) Lower.lim <- 0
  Lower.lim <- round(Lower.lim, 2)
  Lower.lim <- substring(as.character(Lower.lim), 2)

  Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
  Upper.lim <- round(Upper.lim, 2)
  Upper.lim <- substring(as.character(Upper.lim), 2)

  # Print results
  paste0("partialetasq = ", partial.eta, ", [", Lower.lim, ", ", Upper.lim, "]")
}
