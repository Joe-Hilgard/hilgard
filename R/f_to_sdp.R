#' Calculate pooled SD given reported means and F value
#'
#' Sometimes you need to calculate a different contrast than the original authors did.
#' Or you want to make sure that the SDs are plausible given the means and F-value.
#' This function backsolves for the pooled standard deviation given the means, cell size, and F-value.
#'
#' @param m1 Pooled mean of group 1
#' @param m2 Pooled mean of group 2
#' @param f F-value of ANOVA comparing m1 and m2
#' @param n N per cell, assuming equal cell sizes

# note: assumes equal cell sizes. n is n per cell!
f_to_sdp <- function(m1, m2, f, n) {
  x <- abs(m1 - m2)
  t <- sqrt(f)
  z <- sqrt(2/n)
  sdp <- x / (z * t)
  return(sdp)
}
