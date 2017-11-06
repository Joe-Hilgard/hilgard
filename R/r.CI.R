#' Fetch confidence interval given a Pearson r.
#'
#' This method uses the Fisher's Z to make the CI.
#'
#' @param r.equiv Pearson r
#' @param N Total sample size

r.CI <- function(r.equiv, N) {
  zScore <- 1/2 * log((1+r.equiv)/(1-r.equiv))
  z.se <- 1/sqrt(N-3)
  z.low <- r.equiv-1.96*z.se
  z.hi <- r.equiv+1.96*z.se
  r.equiv.low <- (exp(2*z.low)-1)/(exp(2*z.low)+1)
  r.equiv.hi <- (exp(2*z.hi)-1)/(exp(2*z.hi)+1)
  return(data.frame("r" = r.equiv,
                    "r.ci.ll" = r.equiv.low,
                    "r.ci.ul" = r.equiv.hi))
}
