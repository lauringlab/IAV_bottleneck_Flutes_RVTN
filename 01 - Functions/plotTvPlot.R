plotTvPlot <- function(pair_tv, var_thresh, out = 1) {
  df <- pair_tv %>%
    mutate(filtered_out = ifelse(
      donor_freq >= var_thresh &
        donor_freq <= (1 - var_thresh),
      "Included",
      "Excluded"
    ))
  
  if (out == 1) {
    df %>%
      ggplot() +
      geom_point(aes(x = donor_freq, y = recip_freq),
                 size = 1.5,
                 alpha = 0.8) +
      labs(x = "Frequency in donor", y = "Frequency in recipient") +
      theme_bw(base_size = 15)
  } else if (out == 2) {
    df %>%
      na.omit() %>%
      ggplot() +
      geom_point(
        aes(x = donor_freq, y = recip_freq, col = filtered_out),
        size = 1.5,
        alpha = 0.8
      ) +
      scale_color_manual(values = c("#a45851", "black")) +
      labs(x = "Frequency in donor", y = "Frequency in recipient") +
      theme_bw(base_size = 15) +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.87),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", linewidth = 0.5),
        legend.text = element_text(size = 12)
      )
  }
}
