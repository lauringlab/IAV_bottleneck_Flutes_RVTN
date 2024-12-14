plotTvPlotBySeason <- function(pair_tv, var_thresh) {
  df <- pair_tv %>%
    mutate(
      # filtered_out = case_when(
      #   donor_freq == 1 ~ "Excluded",
      #   donor_freq <= var_thresh |
      #     donor_freq >= 1 - var_thresh ~  "Excluded",
      #   recip_freq <= var_thresh |
      #     recip_freq >= 1 - var_thresh &
      #     donor_freq <= var_thresh | donor_freq >= 1 - var_thresh ~  "Excluded",
      #   .default =  "Included"
      # ),
      filtered_out = ifelse(
        donor_freq >= var_thresh & donor_freq <= (1 - var_thresh), "Included", "Excluded"
        ),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22",
        .default = "Other"
      )
    )
  
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
    geom_point(aes(x = donor_freq, y = recip_freq, col = filtered_out), size = 2) +
    facet_wrap(vars(season)) +
    scale_color_manual(values = c("#9A3324", "black")) +
    theme_bw(base_size = 18) +
    labs(x = "Donor Frequency", y = "Recipient Frequency") +
    theme(
      legend.position = "none",
      legend.text = element_text(face = "bold"),
      legend.title = element_blank(),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold", margin = margin(5, 0, 5, 0))
    )
}
