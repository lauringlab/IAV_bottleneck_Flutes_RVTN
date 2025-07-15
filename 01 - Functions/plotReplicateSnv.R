plotReplicateSnv <- function(df) {
  inset <- df %>%
    ggplot(aes(x = rep1_freq, y = rep2_freq)) +
    geom_point(col = "black") +
    theme_minimal() +
    scale_x_continuous(limits = c(0, 0.1), breaks = c(0, 0.05, 0.1)) +
    scale_y_continuous(limits = c(0, 0.1), breaks = c(0, 0.05, 0.1)) +
    theme(axis.title = element_blank())
  
  data.tb <- tibble(
    x = 1,
    y = 1,
    plot = list(
      inset +
        theme_minimal(base_size = 15) +
        theme(axis.title = element_blank())
    )
  )
  
  df %>%
    ggplot(aes(x = rep1_freq, y = rep2_freq)) +
    geom_plot(data = data.tb, aes(x, y, label = plot)) +
    geom_point(col = "black") +
    theme_minimal(base_size = 15) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "iSNV frequency in replicate 1", y = "iSNV frequency in replicate 2") +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"))
}