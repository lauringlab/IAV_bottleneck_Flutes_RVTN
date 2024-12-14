plotWaffleSeason <- function(bottleneck_meta2) {
  season_plot <- bottleneck_meta2 %>%
    group_by(season) %>%
    count() %>%
    ungroup()
  
  ggplot() +
    geom_waffle(
      season_plot,
      mapping = aes(fill = season, values = n),
      size = 2,
      color = "black",
      n_rows = 5
    ) +
    scale_fill_brewer(palette = "Paired") +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 11, 1),
      labels = seq(1, 11, 1)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void(base_size = 24) +
    guides(
      fill = guide_legend(
        nrow = 1,
        title = element_blank(),
        position = "bottom"
      )
    ) +
    theme(
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      legend.text = element_text(face = "bold"),
      legend.justification = "center",
      plot.margin = margin(1, 1, 1.5, 1.2, "cm")
    )
}
