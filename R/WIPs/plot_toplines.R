# Function for plotting toplines
#' @param questions Vector of column names to make toplines for
#' @param design svydesign object
#' @param names Optional vector of pretty names for the questions
#' # TODO: Add column for sorting that is mean approval of non-zero resps
#' # TODO: Add test to toggle between svy and data.frame objecs
#' # CAUTION: This will blow up if questions are measured on different scales

svytab_loop <- function(vars, design){
  prop.table(svytable(as.formula(paste0("~", vars)), design))
}

proptab_loop <- function(var, data){
  with(data, xtabs(as.formula(paste0("~", var)))) %>%
    prop.table()
}

plot_toplines <- function(questions, data, names = questions) {
  # Make proptable for every question
  plotData <- sapply(questions, proptab_loop, data) %>%
    #.[nrow(.):1,] %>% # flip upside-down
    as.data.frame() %>%
    mutate(Response = rownames(.)) %>%
    gather(Question, Proportion, -Response)

  # NB: This considers DK as its labeled value, not NA!
  # Could this be hazardous when DK is coded as -1, 0, 999, etc?
  # This is used only to reorder the scales display by mean -- doesn't influence plotted values
  # Why does this summarize to a single value despite grouping by Question?
  sumData <- group_by(plotData, Question) %>%
    summarize(Mean = sum(Proportion * as.numeric(Response)))

  # What's this do?
  # Add the means to the data, make prettier names, reorder stacked plots according to means,
  # and plot stacked proportion bar plot
  left_join(plotData, sumData, by = "Question") %>%
    mutate(Question = mapvalues(Question, from = questions, to = names)) %>%
    mutate(Question = fct_reorder(Question, Mean)) %>%
    ggplot(aes(x = Question, y = Proportion, fill = Response)) +
    geom_bar(stat = "identity", position = position_stack(reverse = T)) +
    coord_flip()
  #scale_fill_manual(values = c("grey30", "grey50", "grey70", "grey80", "grey95", "black"))
}
