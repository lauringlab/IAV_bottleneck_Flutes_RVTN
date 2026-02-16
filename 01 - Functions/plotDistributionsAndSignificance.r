plotDistributionsAndSignificance <- function(df5, df4, lambda_a_val = 1, lambda_b_val = 3, b_col = "#015b58") {
  a_col <- "#2c6184"
  
  percent <- df5 %>%
    count(sig) %>%
    mutate(
      percent = n / sum(n),
      ymax = cumsum(percent),
      ymin = c(0, head(ymax, n = 1)),
      labelPosition = (ymax + ymin) / 2,
      label = ifelse(
        sig == TRUE,
        paste(round(percent * 100, 2), "%", sep = ""),
        paste("Non-significant\n", round(percent * 100, 2), "%", sep = "")
      )
    )
  a <- df4 %>%
    ggplot() +
    geom_density_ridges2(aes(
      y = useful_order,
      x = val,
      fill = type,
      col = type,
      group = interaction(rep, type)
    ),
    alpha = 0.5) +
    geom_vline(
      xintercept = c(lambda_a_val, lambda_b_val),
      col = c(a_col, b_col),
      lty = 2
    ) +
    coord_flip() +
    scale_color_manual(
      values = c(a_col, b_col),
      labels = c(
        paste("lambda/Nb = ", lambda_a_val, sep = ""),
        paste("lambda/Nb = ", lambda_b_val, sep = "")
      ),
      name = NULL
    ) +
    scale_fill_manual(
      values = c(a_col, b_col),
      labels = c(
        paste("lambda/Nb = ", lambda_a_val, sep = ""),
        paste("lambda/Nb = ", lambda_b_val, sep = "")
      ),
      name = NULL
    ) +
    labs(x = "Bottleneck Size",
         y = "Simulated subgroups (5 pairs per group)") +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    scale_x_continuous(limits = c(1, 18), breaks = c(seq(1, 18, 2))) +
    theme_bw(base_size = 18) +
    theme(
      margins = margin(1, 1, 1, 1, unit = "cm"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.box.background = element_rect(color = "black", size = 1),
      legend.margin = margin(5, 5, 5, 5, unit = "pt")
    )
  
  b <- percent %>%
    ggplot(aes(
      ymax = ymax,
      ymin = ymin,
      xmax = 4,
      xmin = 3,
      fill = sig
    )) +
    geom_rect(col = "black") +
    geom_text(
      data = . %>% filter(sig == TRUE),
      x = 2,
      aes(y = labelPosition, label = label)
    ) +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void(base_size = 18) +
    scale_fill_manual(values = c("TRUE" = "#f5db99", "FALSE" = "grey")) +
    theme(legend.position = "none")
  
  a + b
}