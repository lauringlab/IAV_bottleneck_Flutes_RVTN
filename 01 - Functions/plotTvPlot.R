plotTvPlot <- function(pair_tv, var_thresh, out = 1) {
  df <- pair_tv %>%
    mutate(
      filtered_out = ifelse(
        donor_freq >= var_thresh &
          donor_freq <= (1 - var_thresh),
        "Included",
        "Excluded"
      ),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22",
        .default = "Other"
      )
    )
  
  if (out == 1) {
    df %>%
      ggplot() +
      geom_point(aes(x = donor_freq, y = recip_freq),
                 size = 5,
                 alpha = 0.8) +
      theme_bottleneck() +
      labs(x = "Donor Frequency", y = "Recipient Frequency") +
      guides(col = "none") +
      scale_y_continuous(limits = c(-0.05, 1.05)) +
      scale_shape_manual(values = c(15, 16, 17, 18)) +
      theme(
        legend.position = "right",
        legend.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", margin = margin(5, 0, 5, 0))
      )
  } else if (out == 2) {
    df %>%
      ggplot() +
      annotate(
        "rect",
        xmin = var_thresh,
        xmax = 1 - var_thresh,
        ymin = -0.03,
        ymax = 1.03,
        alpha = 0.1,
        fill = "black",
        col = "black"
      ) +
      geom_point(
        aes(x = donor_freq, y = recip_freq, col = filtered_out),
        size = 5,
        alpha = 0.8
      ) +
      scale_color_manual(values = c("#9A3324", "black")) +
      scale_y_continuous(limits = c(-0.05, 1.05)) +
      theme_bottleneck() +
      labs(x = "Donor Frequency", y = "Recipient Frequency") +
      guides(col = "none") +
      scale_shape_manual(values = c(15, 16, 17, 18)) +
      theme(
        legend.position = "right",
        legend.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", margin = margin(5, 0, 5, 0))
      )
  } else if (out == 3) {
    df %>%
      ggplot() +
      annotate(
        "rect",
        xmin = var_thresh,
        xmax = 1 - var_thresh,
        ymin = -0.03,
        ymax = 1.03,
        alpha = 0.1,
        fill = "black",
        col = "black"
      ) +
      geom_point(
        aes(
          x = donor_freq,
          y = recip_freq,
          col = filtered_out,
          shape = season
        ),
        size = 5,
        alpha = 0.8
      ) +
      scale_color_manual(values = c("#9A3324", "black")) +
      theme_bottleneck() +
      labs(x = "Donor Frequency", y = "Recipient Frequency") +
      guides(col = "none") +
      scale_shape_manual(values = c(15, 16, 17, 18)) +
      theme(
        legend.position = "right",
        legend.text = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", margin = margin(5, 0, 5, 0))
      )
  }
}
