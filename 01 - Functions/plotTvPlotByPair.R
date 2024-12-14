plotTvPlotByPair <- function(pair_tv, var_thresh, out = 1) {
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
  
  df %>%
    filter(filtered_out == "Included") %>% 
    ggplot() +
    geom_point(aes(x = donor_freq, y = recip_freq, fill = season),
               alpha = 0.8, col = "black", shape = 21, size = 2) +
    # theme_bottleneck() +
    labs(x = "Donor Frequency", y = "Recipient Frequency") +
    guides(col = "none") +
    facet_wrap(vars(pair_id), nrow = 5, ncol = 11) +
    scale_fill_brewer(palette = "Paired") +
    scale_x_continuous(expand = c(0.05, 0.05)) +
    scale_y_continuous(expand = c(0.05, 0.05)) +
    theme_void(base_size = 18) +
    coord_fixed(1) +
    geom_text(aes(label = pair_id, x = 1, y = 1), hjust = 1, fontface = "bold") +
    theme(
      legend.position = "none",
      legend.text = element_text(face = "bold"),
      legend.title = element_blank(),
      # axis.text = element_text(face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
      strip.text = element_blank()
    ) 
}
