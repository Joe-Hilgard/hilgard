#' Conduct a PET test. This is a WIP!
#'
#' @param data dataset
#' @param error error model. Choices are "additive" (a la rma) or "multiplicative" (a la lm)
#' @param yi observed effect
#' @param sei observed standard error
#' @param method TODO

PET <- function(yi, sei, data, error = "additive", ...) {
  if (error == "additive") {
    petOut = rma(yi = yi,
                 sei = sei,
                 mods = ~sei,
                 data = data,
                 ...)
  }
  if (error == "multiplicative") {
    petOut = lm(yi ~ sei,
                weights = 1/sei,
                data = data,
                ...)
  }
  return(petOut)
}
