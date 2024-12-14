plotBottleneckSizeByMetadata <- function(bottleneck_by_metadata) {
  bottleneck_by_metadata %>%
    ggplot() +
    geom_col(aes(x = x, y = 10 * max_LL),
             col = "black",
             fill = "white") +
    geom_col(aes(x = x, y = -sample_size),
             size = 2,
             fill = "grey") +
    geom_segment(
      aes(
        x = x,
        xend = x,
        y = 10 * lower_CI,
        yend = 10 * upper_CI
      ),
      lwd = 1,
      col = "grey4"
    ) +
    geom_point(aes(
      x = x,
      y = (10 * max_LL) + 5,
      shape = as.factor(no_conf)
    ), size = 4) +
    geom_hline(
      yintercept = 20,
      lty = 2,
      col = "red",
      lwd = 1
    ) +
    scale_shape_manual(values = c(NA, 13)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = c(-40, -30, -20, -10, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      labels = c(40, 30, 20, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      limits = c(-41, 105)
    ) +
    scale_x_continuous(
      breaks = seq(1, 18, 1),
      labels = levels,
      expand = c(0, 0),
      limits = c(0.5, 18.5)
    ) +
    geom_vline(xintercept = c(4.5, 6.5, 10.5, 14.5)) +
    theme_bottleneck() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.title = element_blank(),
    )
}