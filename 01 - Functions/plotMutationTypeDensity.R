plotMutationTypeDensity <- function(df) {
  col_palette <- c("#e69b99", "#24492e")

  
  df %>%
    ggplot(aes(x = avg_freq, fill = mutation_type, col = mutation_type)) +
    geom_density(alpha = 0.5) +
    theme_minimal(base_size = 15) +
    scale_fill_manual(values = col_palette,
                      labels = c("Non-synonymous", "Synonymous")) +
    scale_color_manual(values = col_palette,
                       labels = c("Non-synonymous", "Synonymous")) +
    labs(
      x = "Frequency",
      y = "Density",
      fill = element_blank(),
      col = element_blank()
    ) +
    theme(legend.position = "inside", 
      legend.position.inside = c(0.83, 0.9))
}