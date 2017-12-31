#' Pool standard deviations
#'
#' This function takes in a vector of standard deviations and sample sizes
#' to create a pooled standard deviation
#' @param sds Vector of standard deviations
#' @param ns Vector of sample sizes
#'
#' @export

pool.sd <- function (sds, ns) {
  SSlist = sds^2 %*% (ns-1)
  pool.var = sum(SSlist) / sum(ns-1)
  return(sqrt(pool.var))
}
