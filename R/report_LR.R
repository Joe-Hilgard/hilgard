#' Report manually-made likelihood-ratio chisq-test
#'
#' Report an LR chisq-test as chisq(df) = chisq, p = .###.
#' I am considering deprecating this and replacing it with a generic
#' reporter function for manually-supplied chi-sqs or stats in general.
#' @param model Model object from which to extract chisq-value.
#' @param effect Parameter with chisq-value of interest
#' @template imports
#'
#' @export

report_LR <- function(row) {

  df <- row$df

  chisq <- row$LR.chisq
  chisq <- numformat(chisq)

  p <- row$p.value
  p <- fix_p(p)

  output <- paste0("*chisq*(", df, ") = ", chisq, ", ", p)
  return(output)
}
