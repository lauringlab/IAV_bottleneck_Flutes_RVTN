plotBottleneckWithNumberOfVariants <- function(bottleneck_meta2 = bottleneck_meta2,
                                               dup = dup,
                                               bottleneck_matrix = out) {
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
    scale_y_continuous(
      breaks = c(-40, -30, -20, -10, 5, 25, 50, 75),
      labels = c(40, 30, 20, 10, 1, 5, 10, 15),
      expand = c(0, 0),
      limits = c(-41, 76)
    ) +
    geom_hline(yintercept = bottleneck_matrix$max_LL * 5,
               col = "red",
               lwd = 1) +
    annotate(
      "text",
      y = bottleneck_matrix$max_LL * 5 + 10,
      x = 45,
      label = paste('"Weighted average"\nBottleneck size:\n', bottleneck_matrix$max_LL, " (CI:", bottleneck_matrix$lower_CI, ", ", bottleneck_matrix$upper_CI, ")", sep = ""),
      size = 8,
      col = "red",
      fontface = "bold",
      lineheight = 0.8) +
    geom_hline(yintercept = 0, col = "black", lwd = 1) +
    scale_fill_manual(values = c("black", "white")) +
    geom_text(
      tibble(),
      mapping = aes(
        x = c(1.4, 16.4, 26.4, 51.3),
        y = rep(53, 4),
        label = c("2017-2018", "2018-2019", "2019-2020", "2021-2022")
      ),
      angle = 90,
      size = 6,
      hjust = 0,
      vjust = 0,
      fontface = "bold"
    ) +
    geom_vline(xintercept = c(1.5, 16.5, 26.5), lty = 2) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_bottleneck() +
    theme(
      legend.position = "top",
      legend.justification = "right",
      legend.box.spacing = unit(0, "pt"),
      legend.title = element_blank(),
      axis.title = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )
}