plotISnvDistribution <- function(df = df, bySeason = FALSE) {
  season_colors <- pnw_palette("Cascades", n = 4, type = "discrete")
  if (bySeason == TRUE) {
    p1 <- df %>%
      ggplot(aes(x = avg_freq, fill = as.factor(season))) +
      geom_histogram(
        binwidth = 0.05,
        boundary = 0,
        position = "dodge",
        col = "white"
      ) +
      theme_minimal(base_size = 15) +
      scale_x_continuous(breaks = seq(0, 0.5, 0.1)) +
      # scale_y_break(breaks = c(50, 100), scale = 0.25) +
      scale_fill_manual(
        values = season_colors,
        labels = c("2017-2018", "2018-2019", "2019-2020", "2021-2022")
      ) +
      labs(x = "iSNV frequency", y = "Number of iSNV", fill = "Season")
    
    p2 <- df %>%
      filter(!is.na(avg_freq)) %>%
      group_by(sample, season) %>%
      dplyr::count() %>%
      ggplot(aes(x = n, fill = as.factor(season))) +
      geom_histogram(
        boundary = 0,
        position = "dodge",
        col = "white",
        binwidth = 5
      ) +
      scale_x_continuous(breaks = seq(0, 40, 5)) +
      scale_fill_manual(
        values = season_colors,
        labels = c("2017-2018", "2018-2019", "2019-2020", "2021-2022")
      ) +
      theme_minimal(base_size = 15) +
      labs(x = "iSNV per specimen", y = "Number of specimens", fill = "Season")
  } else {
    p1 <- df %>%
      ggplot(aes(x = avg_freq)) +
      geom_histogram(
        binwidth = 0.05,
        boundary = 0,
        position = "dodge",
        col = "white",
        fill = "grey80"
      ) +
      theme_minimal(base_size = 15) +
      scale_x_continuous(breaks = seq(0, 0.5, 0.1)) +
      labs(x = "iSNV frequency", y = "Number of iSNV")
    
    p2 <- df %>%
      filter(!is.na(avg_freq)) %>%
      group_by(sample, season) %>%
      dplyr::count() %>%
      ggplot(aes(x = n)) +
      geom_histogram(
        boundary = 0,
        position = "dodge",
        col = "white",
        fill = "grey80",
        binwidth = 2
      ) +
      scale_x_continuous(breaks = seq(0, 40, 4)) +
      scale_y_continuous(breaks = seq(0, 30, 4)) +
      theme_minimal(base_size = 15) +
      labs(x = "iSNV per specimen", y = "Number of specimens")
  }
  
  p1 + p2 + plot_layout(guides = "collect")
}
