plotBottleneckWithNumberOfVariants <- function(bottleneck_meta2 = bottleneck_meta2,
                                               bottleneck_matrix = out) {
  bottleneck_meta2 <- bottleneck_meta2 %>% 
    mutate(pair_id = row_number())
  
  season_breaks <- bottleneck_meta2 %>% 
    select(pair_id, season) %>% 
    group_by(season) %>% 
    summarize(max = max(pair_id)) %>% 
    select(max) %>% 
    as_vector()
  
  cropped <- bottleneck_meta2 %>% 
    dplyr::filter(max_LL > 15)
  
  ggplot() +
    geom_col(
      bottleneck_meta2,
      mapping = aes(x = pair_id, y = 5 * max_LL, fill = subtype),
      col = "black"
    ) +
    geom_col(
      bottleneck_meta2,
      mapping = aes(x = pair_id, y = -n_variants),
      alpha = 0.25
    ) +
    geom_hline(yintercept = 0, col = "black", lwd = 1) +
    geom_point(data = cropped,
               mapping = aes(x = pair_id, y = 77, shape = "cropped"),
               col = "black", shape = 8, size = 2) +
    geom_text(
      tibble(),
      mapping = aes(
        x = season_breaks + c(0.4, 0.4, 0.4, 0.3),
        y = rep(60, 4),
        label = c("2017-2018", "2018-2019", "2019-2020", "2021-2022")
      ),
      angle = 90,
      size = 5,
      hjust = 0,
      vjust = 0
    ) +
    geom_vline(xintercept = season_breaks[1:3] + 0.5, lty = 2) +
    scale_fill_manual(values = c("#81a9ad", "white")) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq(1, 65, 5)) +
    scale_y_continuous(
      breaks = c(-40, -30, -20, -10, 5, 25, 50, 75),
      labels = c(40, 30, 20, 10, 1, 5, 10, 15),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(-40, 80)) +
    labs(y = "  Number of variants used                                 Bottleneck size") +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.99, 0.05),
      legend.justification = "right",
      legend.box.spacing = unit(0, "pt"),
      legend.background = element_rect(colour = "black"),
      legend.title = element_blank(),
      axis.title.y = element_text(hjust = 0),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )
}