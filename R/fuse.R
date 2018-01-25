#' Fuse cell means and pool SDs to turn a many cells into two cells
#'
#' Often you need to calculate an effect size from a 2×2 (or more complex!) study design.
#' Use this function to combine the relevant cells into just two cells for entry into your spreadsheet.
#' @param m1 Vector of means to pool into Group 1
#' @param sd1 Vector of SDs to pool into Group 1
#' @param n1 Vector of cell ns to pool into Group 1
#' @param m2 Vector of means to pool into Group 2
#' @param sd2 Vector of SDs to pool into Group 2
#' @param n2 Vector of cell ns to pool into Group 2
#'
#' @export
#' @examples
#' fuse(m1 = c(2.9, 3.8),
#'     m2 = c(4.9, 2.8),
#'     sd1 = c(1.3, 1.8),
#'     sd2 = c(0.7, 1.3),
#'     n1 = c(25, 25),
#'     n2 = c(25, 25))

fuse <- function(m1, sd1, n1, m2, sd2, n2) {
  m1i <- weighted.mean(m1, n1)
  m2i <- weighted.mean(m2, n2)
  sd1i <- pool.sd(sd1, n1)
  sd2i <- pool.sd(sd2, n2)
  n1i <- sum(n1)
  n2i <- sum(n2)
  df <- data.frame(m1i, m2i, sd1i, sd2i, n1i, n2i)
  return(metafor::escalc("SMD", m1i = m1i, m2i = m2i, sd1i = sd1i, sd2i = sd2i, n1i = n1i, n2i = n2i,
                data = df))
}
