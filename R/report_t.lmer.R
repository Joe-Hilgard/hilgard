#' Report a t-test from an lmer model
#'
#' Note that I have no idea what df to supply pt() and that Anova() probably
#' uses chi-square LR tests for good reason... so this may be a bad idea.
#'
#' Reports a t-test from an lmer object as t = t, p = ###.
#' @param model lmer model object from which to extract t-value.
#' @param effect Parameter with t-value of interest
#' @template imports

report_t.lmer <- function(model, effect) {
  frame <- tidy(summary(model)$coefficients)
  names(frame) <- c("term", "estimate", "std.error", "statistic")
  frame$p.value <- 2*pt(abs(frame$statistic), df = 1e3, lower.tail = F)
  t <- with(frame, statistic[term == effect]) %>%
    round(2)
  p <- with(frame, p.value[term == effect]) %>% fix_p
  paste0("*t* = ", t, ", ", p) %>%
    return()
}
