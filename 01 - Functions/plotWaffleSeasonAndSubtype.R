plotWaffleSeasonAndSubtype <- function(bottleneck_meta2) {
  season_plot <- bottleneck_meta2 %>%
    group_by(season) %>%
    count() %>%
    ungroup()
  
  test <- tibble(x = rep(1:11, 5),
                 y = c(rep(1, 11), rep(2, 11), rep(3, 11), rep(4, 11), rep(5, 11))) %>%
    arrange(x, y) %>%
    mutate(pair_id = row_number()) %>%
    filter(pair_id <= 51) %>%
    full_join(bottleneck_meta2)
  
  ggplot() +
    geom_waffle(
      season_plot,
      mapping = aes(fill = season, values = n),
      size = 2,
      color = "black",
      n_rows = 5
    ) +
    geom_point(test,
               mapping = aes(x = x, y = y, shape = subtype),
               size = 4) +
    scale_fill_brewer(palette = "Paired") +
    scale_shape_manual(values = c(1, 19)) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 11, 1),
      labels = seq(1, 11, 1)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void(base_size = 24) +
    guides(
      shape = guide_legend(
        nrow = 2,
        title = element_blank(),
        position = "right"
      ),
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