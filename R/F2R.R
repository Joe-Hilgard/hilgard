#' Convert an F-test with 1 degree of freedom to effect size r.
#'
#' @param Fstat The F-value of a test
#' @param N The total sample size
#' @param width Width of confidence interval
#' @param neg Indicates when effect size is in direction such that r < 0
#' @export

F2R = function(Fstat, N, width=.95, neg=F) {
  r.equiv = sqrt(Fstat/(Fstat + N - 2)) # is this where the loss of fidelity happens?
  if(neg==T) {r.equiv=-(r.equiv)}
  zScore = 1/2 * log((1+r.equiv)/(1-r.equiv))
  z.se = 1/sqrt(N-3)
  margin = -qnorm((1-width)/2)
  z.low = r.equiv-margin*z.se
  z.hi = r.equiv+margin*z.se
  r.equiv.low = (exp(2*z.low)-1)/(exp(2*z.low)+1)
  r.equiv.hi = (exp(2*z.hi)-1)/(exp(2*z.hi)+1)
  return(data.frame("r" = r.equiv,
                    "r.ci.ll" = r.equiv.low,
                    "r.ci.ul" = r.equiv.hi,
                    #"SE" = "???",
                    "Z" = zScore,
                    "z.se" = z.se))
}
