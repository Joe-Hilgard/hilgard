#' Convert a t-test to effect size r
#'
#' @param tstat t-statistic to be used
#' @param N total sample size
#' @export

t2R = function(tstat, N) {
  neg = tstat<0
  Fstat = tstat^2
  r.equiv = sqrt(Fstat/(Fstat + N - 2))
  if(neg==T) {r.equiv=-(r.equiv)}
  zScore = 1/2 * log((1+r.equiv)/(1-r.equiv))
  z.se = 1/sqrt(N-3)
  z.low = r.equiv-1.96*z.se
  z.hi = r.equiv+1.96*z.se
  r.equiv.low = (exp(2*z.low)-1)/(exp(2*z.low)+1)
  r.equiv.hi = (exp(2*z.hi)-1)/(exp(2*z.hi)+1)
  # print(paste("Point estimate:", r.equiv))
  # print(paste("95% CI: [", r.equiv.low, ", ", r.equiv.hi, "]", sep=""))
  return(data.frame("r" = r.equiv,
                    "r.ll" = r.equiv.low,
                    "r.ul" = r.equiv.hi))
}
