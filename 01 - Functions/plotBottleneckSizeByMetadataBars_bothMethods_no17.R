plotBottleneckSizeByMetadataBars_bothMethods_no17 <- function(isnvData, clonalData) {
  levels <- c(
    "Overall",
    "18/19",
    "19/20",
    "21/22",
    "H1N1",
    "H3N2",
    "A/A",
    "A/C",
    "C/A",
    "C/C",
    "M/M",
    "M/F",
    "F/M",
    "F/F",
    "V/U",
    "V/V",
    "U/U",
    "V/U"
  )
  
  iSNV_method <- isnvData %>%
    dplyr::rename(metric = level) %>%
    mutate(
      metric = case_when(
        metric == "18/19" ~ "18",
        metric == "19/20" ~ "19",
        metric == "21/22" ~ "21",
        metric == "Adult-to-adult" ~ "Adult / Adult",
        metric == "Adult-to-child" ~ "Adult / Child",
        metric == "Child-to-adult" ~ "Child / Adult",
        metric == "Child-to-child" ~ "Child / Child",
        metric == "Both Female" ~ "Female / Female",
        metric == "Both male" ~ "Male / Male",
        metric == "Female-to-male" ~ "Female / Male",
        metric == "Male-to-female" ~ "Male / Female",
        metric == "Both" ~ "Vaccinated / Vaccinated",
        metric == "Neither" ~ "Unknown or unvaccinated / Unknown or unvaccinated",
        metric == "Donor only" ~ "Vaccinated / Unknown or unvaccinated",
        metric == "Recipient only" ~ "Unknown or unvaccinated / Vaccinated",
        metric == "overall" ~ "Overall",
        .default = metric
      )
    ) %>%
    dplyr::rename(samples = sample_size, estimate = max_LL) %>%
    mutate(method = "snv") %>%
    dplyr::select(metric, estimate, lower_CI, upper_CI, samples, method)
  
  clonal_method <- clonalData %>%
    dplyr::rename(samples = sample_size,
                  estimate = max_LL,
                  metric = level) %>%
    mutate(method = "clonal") %>%
    dplyr::select(metric, estimate, lower_CI, upper_CI, samples, method)
  
  
  df <- bind_rows(iSNV_method, clonal_method)
  
  df2 <- df %>%
    dplyr::mutate(
      metric = case_when(
        metric == "17" | metric == "17/18" ~ "2017-2018",
        metric == "18" | metric == "18/19" ~ "2018-2019",
        metric == "19" | metric == "19/20" ~ "2019-2020",
        metric == "21" | metric == "21/22" ~ "2021-2022",
        .default = metric
      )
    ) %>%
    dplyr::mutate(metric = factor(
      metric,
      levels = c(
        "Overall",
        "2017-2018",
        "2018-2019",
        "2019-2020",
        "2021-2022",
        "H1N1",
        "H3N2",
        "Adult / Adult",
        "Adult / Child",
        "Child / Adult",
        "Child / Child",
        "Male / Male",
        "Male / Female",
        "Female / Male",
        "Female / Female",
        "Vaccinated / Unknown or unvaccinated",
        "Vaccinated / Vaccinated",
        "Unknown or unvaccinated / Unknown or unvaccinated",
        "Unknown or unvaccinated / Vaccinated"
      )
    )) %>%
    dplyr::mutate(method = ifelse(method == "snv", "iSNV", "Clonal")) %>%
    group_by(method) %>%
    dplyr::mutate(level = row_number())
  
p <- ggplot() +
    geom_col(
      df2,
      mapping = aes(
        x = level,
        y = -(samples / 20),
        col = method
      ),
      position = "dodge",
      fill = "grey",
      lwd = 1
    ) +
    geom_col(
      df2,
      mapping = aes(x = level, y = estimate, fill = method),
      position = "dodge",
      alpha = 0.5,
      color = "white",
      lwd = 1.5
    ) +
    geom_errorbar(df2 %>% filter(method == "iSNV"),
                  mapping = aes(x = level + 0.22, ymin = lower_CI, ymax = upper_CI), width = 0.2) +
    geom_errorbar(df2 %>% filter(method == "Clonal"),
                  mapping = aes(x = level - 0.22, ymin = lower_CI, ymax = upper_CI), width = 0.2) +
    geom_hline(yintercept = 0,
               lwd = 1,
               lty = 1) +
    geom_vline(xintercept = c(1.5, 4.5, 6.5, 10.5, 14.5),
               lwd = 1) +
    geom_text(aes(
      x = c(3, 5.5, 8.5, 12.5, 16.5),
      y = rep(2, 5),
      label = c("Season", "Subtype", "Age", "Sex", "Vaccination")
    ), size = 7) +
    scale_x_continuous(
      breaks = seq(1, 18, 1),
      labels = levels,
      limits = c(0.5, 18.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = c(-3.5, -3, -2.5, -2, -1.5, -1, -0.5, 1, 2),
      labels = c(70, 60, 50, 40, 30, 20, 10, 1, 2),
      limits = c(-3.5, 2.5),
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("#89689d", "#2c6184")) +
    scale_color_manual(values = c("white", "white")) +
    theme_classic(base_size = 20) +
    labs(y = "             Number of pairs                    Bottleneck size") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(hjust = 0),
      legend.position = "inside",
      legend.position.inside = c(0.94, 0.1),
      legend.title = element_blank(),
      legend.box.background = element_rect(color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
  
  return(p)
}