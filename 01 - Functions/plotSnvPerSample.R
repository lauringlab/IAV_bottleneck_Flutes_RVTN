plotSnvPerSample <- function(ind_snv, measure = median, faceted = FALSE) {
  df <- ind_snv %>%
    group_by(sample, season) %>%
    tally() %>% 
    mutate(season = case_when(
      season == 17 ~ "17/18",
      season == 18 ~ "18/19",
      season == 19 ~ "19/20",
      season == 21 ~ "21/22",
      .default = "ERROR"
    ))
  
  if (faceted == TRUE) {
    mean <- df %>% 
      group_by(season) %>% 
      summarise(mean = mean(n))
    
    median <- df %>% 
      group_by(season) %>% 
      summarize(median = median(n))
    
    df %>%
      ggplot(aes(x = n, fill = as.factor(season), group = season)) +
      geom_histogram(
        binwidth = 1,
        boundary = 0,
        closed = "left",
        col = "grey2"
      ) +
      scale_x_continuous(
        limits = c(0, 39),
        breaks = seq(0, 40, 3),
        expand = c(0, 0)
      ) +
      scale_y_continuous(limits = c(0, 15),
                         breaks = pretty,
                         expand = c(0, 0)) +
      MetBrewer::scale_fill_met_d("Egypt") +
      xlab("iSNV per specimens") +
      ylab("Number of specimens") +
      theme_bottleneck() +
      labs(fill = "Season") +
      theme(legend.position = "bottom")
    
  } else {
    mean <- mean(df$n)
    median <- median(df$n)
    
    df %>%
      ggplot(aes(x = n)) +
      geom_histogram(
        binwidth = 3,
        boundary = 0,
        closed = "left",
        fill = "#FF86FF",
        col = "white"
      ) +
      scale_x_continuous(
        limits = c(0, 39),
        breaks = seq(0, 40, 3),
        expand = c(0, 0)
      ) +
      scale_y_continuous(limits = c(0, 30),
                         breaks = pretty,
                         expand = c(0, 0)) +
      # geom_vline(xintercept = measure,
      #            col = "#D86018",
      #            lwd = 1.5) +
      xlab("iSNV per specimens") +
      ylab("Number of specimens") +
      theme_bottleneck()
  }
}