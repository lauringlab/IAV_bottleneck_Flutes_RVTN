plotClonalDistribution <- function(pair_clonal) {
  pair_clonal_plot <- pair_clonal %>%
    group_by(clonal_diff) %>%
    tally()
  
  pair_clonal_plot %>%
    ggplot(aes(x = clonal_diff, y = n)) +
    geom_col(col = "white", fill = "grey60") +
    # scale_x_continuous(labels = seq(0, 14, 2), breaks = seq(-0.5, 13.5, 2)) +
    scale_x_continuous(labels = seq(0, 5, 1), breaks = seq(0, 5, 1)) +
    theme_bw(base_size = 15) +
    labs(x = "Number of clonal differences", y = "Frequency")
}

plotClonalDistribution_endTrim <- function(pair_clonal) {
  pair_clonal_plot <- pair_clonal %>%
    group_by(clonal_diff) %>%
    tally()
  
  pair_clonal_plot %>%
    ggplot(aes(x = clonal_diff, y = n)) +
    geom_col(col = "white", fill = "grey60") +
    scale_x_continuous(labels = seq(0, 8, 2), breaks = seq(-0.5, 7.5, 2)) +
    theme_bw(base_size = 15) +
    labs(x = "Number of clonal differences", y = "Frequency")
}