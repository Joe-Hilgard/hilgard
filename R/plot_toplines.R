# Function for plotting toplines
#' @param questions Vector of column names to make toplines for
#' @param design svydesign object
#' @param names Optional vector of pretty names for the questions
#' # TODO: Add column for sorting that is mean approval of non-zero resps
plot_toplines <- function(questions, design, names = questions) {

  svytab_loop <- function(vars, design){
    prop.table(svytable(as.formula(paste0("~", vars)), design))
  }

  plotData <- sapply(questions, svytab_loop, design) %>%
    #.[nrow(.):1,] %>% # flip upside-down
    as.data.frame() %>%
    mutate(Response = rownames(.)) %>%
    gather(Question, Proportion, -Response)

  # NB: This considers DK as zero, not NA!
  sumData <- group_by(plotData, Question) %>%
    summarize(Mean = sum(Proportion * as.numeric(Response)))

  left_join(plotData, sumData, by = "Question") %>%
    mutate(Question = mapvalues(Question, from = questions, to = names)) %>%
    mutate(Question = fct_reorder(Question, Mean)) %>%
    ggplot(aes(x = Question, y = Proportion, fill = Response)) +
    geom_bar(stat = "identity", position = position_stack(reverse = T)) +
    coord_flip()
  #scale_fill_manual(values = c("grey30", "grey50", "grey70", "grey80", "grey95", "black"))
}
