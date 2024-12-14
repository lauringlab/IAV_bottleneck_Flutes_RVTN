plotWaffleBlank <- function(bottleneck_meta2) {
  season_plot <- bottleneck_meta2 %>%
    group_by(season) %>%
    count() %>%
    ungroup()
  
  season_plot %>%
    mutate(fill = "t") %>%
    ggplot() +
    geom_waffle(
      mapping = aes(fill = fill, values = n),
      size = 2,
      color = "black",
      n_rows = 5
    ) +
    scale_fill_manual(values = "grey") +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 11, 1),
      labels = seq(1, 11, 1)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void(base_size = 24) +
    theme(legend.position = "none",
          plot.margin = margin(1, 1, 1.5, 1.2, "cm"))
}
