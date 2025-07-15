plotBottleneckSizeByMetadataBars_bothMethods2 <- function(isnvData, clonalData){
  levels <- c("Overall",
              "17/18", "18/19", "19/20", "21/22",
              "H1N1", "H3N2",
              "A/A", "A/C", "C/A", "C/C",
              "M/M", "M/F", "F/M", "F/F",
              "V/U", "V/V", "U/U", "V/U")
  
  iSNV_method <- isnvData %>% 
    dplyr::rename(metric = level) %>% 
    mutate(metric = case_when(
      metric == "17/18" ~ "17",
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
    )) %>% 
    dplyr::rename(samples = sample_size,
                  estimate = max_LL) %>% 
    mutate(method = "snv") %>% 
    dplyr::select(metric, estimate, samples, method)
  
  clonal_method <- clonalData %>% 
    dplyr::rename(samples = samples,
                  estimate = meanNb) %>% 
    mutate(method = "clonal") %>% 
    dplyr::select(metric, estimate, samples, method)
  
  
  df <- bind_rows(iSNV_method, clonal_method)
  
  df2 <- df %>% 
    dplyr::mutate(metric = case_when(
      metric == "17" ~ "2017-2018",
      metric == "18" ~ "2018-2019",
      metric == "19" ~ "2019-2020",
      metric == "21" ~ "2021-2022",
      .default = metric
    )) %>% 
    dplyr::mutate(metric = factor(metric,
                                  levels = c("Overall",
                                             "2017-2018", "2018-2019", "2019-2020", "2021-2022",
                                             "H1N1", "H3N2",
                                             "Adult / Adult", "Adult / Child", "Child / Adult", "Child / Child",
                                             "Male / Male", "Male / Female", "Female / Male", "Female / Female",
                                             "Vaccinated / Unknown or unvaccinated",
                                             "Vaccinated / Vaccinated",
                                             "Unknown or unvaccinated / Unknown or unvaccinated",
                                             "Unknown or unvaccinated / Vaccinated"))) %>% 
    dplyr::mutate(method = ifelse(method == "snv", "iSNV", "Clonal")) %>% 
    group_by(method) %>% 
    dplyr::mutate(level = row_number())
  
  p <- ggplot() +
    geom_col(df2, 
             mapping = aes(
               x = level,
               y = -(samples / 20),
               col = method
             ),
             position = "dodge",
             fill = "grey",
             lwd = 1
    ) +
    geom_col(df2, mapping = aes(x = level, y = estimate, fill = method),
             position = "dodge",
             alpha = 0.5,
             color = "white", lwd = 1.5) +
    geom_hline(
      yintercept = 0,
      lwd = 1,
      lty = 1
    ) +
    geom_vline(xintercept = c(1.5, 5.5, 7.5, 11.5, 15.5), lwd = 1) +
    geom_text(aes(x = c(3.5, 6.5, 9.5, 13.5, 17.5), y = rep(1.5, 5),
                  label = c("Season", "Subtype", "Age", "Sex", "Vaccination")),
              size = 7) +
    scale_x_continuous(
      breaks = seq(1, 19, 1),
      labels = levels,
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      breaks = c(-3.5, -3, -2.5, -2, -1.5, -1, -0.5, 1),
      labels = c(70, 60, 50, 40, 30, 20, 10, 1),
      limits = c(-3.5, 2),
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("#89689d", "#2c6184")) +
    scale_color_manual(values = c("white", "white")) +
    theme_classic(base_size = 20) +
    labs(y = "                Number of pairs                     Bottleneck size") +
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