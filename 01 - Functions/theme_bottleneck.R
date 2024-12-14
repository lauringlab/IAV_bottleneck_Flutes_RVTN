theme_bottleneck <- function() {
  theme_bw(base_size = 24) +
    theme(
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
      legend.text = element_text(size = 24, face = "bold"),
      legend.title = element_text(face = "bold")
    )
}
