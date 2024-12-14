plotSnvFrequencyPerSample <- function(ind_snv,
                                      var_thresh = 0.02,
                                      faceted = TRUE) {
  df <- ind_snv %>%
    mutate(
      excluded = ifelse(avg_freq < var_thresh, "No", "Yes"),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22",
        .default = "ERROR"
      )
    )
  
  if (faceted == TRUE) {
    df %>%
      ggplot(aes(
        x = avg_freq,
        # alpha = as.factor(excluded),
        fill = as.factor(season)
      )) +
      geom_histogram(
        binwidth = var_thresh,
        boundary = 0,
        closed = "left",
        col = "grey2"
      ) +
      # scale_alpha_manual(values = c(0.5, 1)) +
      xlab("SNV frequency") +
      ylab("Occurrences in data set") +
      geom_vline(xintercept = var_thresh, lwd = 2) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 0.5),
        breaks = seq(0, 0.5, (var_thresh * 2))
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      MetBrewer::scale_fill_met_d("Egypt") +
      # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
      #               labels = trans_format("log10", math_format(10^.x)),
      #               expand = c(0, 0)) +
      labs(fill = "Season", alpha = "Included in\nanalysis set") +
      theme_bottleneck() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
    
  } else {
    df %>%
      ggplot(aes(x = avg_freq)) +
      geom_histogram(
        binwidth = 0.02,
        boundary = 0,
        closed = "left",
        col = "grey2"
      ) +
      # scale_fill_manual(values = c("#9A3324", "lightgrey")) +
      xlab("SNV frequency") +
      ylab("Number of SNV") +
      # geom_vline(xintercept = 0.02, lwd = 1.5) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 0.5),
        breaks = seq(0, 0.5, 0.1)
      ) +
      scale_y_log10(expand = c(0, 0)) +
      theme_bottleneck() +
      labs(color = "Included in\nanalysis set") +
      theme(legend.position = "bottom")
  }
}
