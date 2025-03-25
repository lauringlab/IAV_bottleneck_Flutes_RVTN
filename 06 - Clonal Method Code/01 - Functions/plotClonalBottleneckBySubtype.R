plotClonalBottleneckBySubtype <- function(pathToBottlenecksByFactor, pathToClonalNumSamples, showMu = TRUE) {
  levels <- c(
    "Overall",
    "17/18",
    "18/19",
    "19/20",
    "21/22",
    "H1N1",
    "H3N2",
    "Adult-to-adult",
    "Adult-to-child",
    "Child-to-adult",
    "Child-to-child",
    "Both male",
    "Male-to-female",
    "Female-to-male",
    "Both Female",
    "Donor only",
    "Both",
    "Neither",
    "Recipient only"
  )

  bottlenecks_by_factor <- read_csv(pathToBottlenecksByFactor) %>%
    filter(id == "max") %>%
    select(meanNb, mu, lambda, meanN, type) %>%
    mutate(
      type = case_when(
        type == 17 ~ "17/18",
        type == 18 ~ "18/19",
        type == 19 ~ "19/20",
        type == 21 ~ "21/22",
        type == "adultToAdult" ~ "Adult-to-adult",
        type == "adultToChild" ~ "Adult-to-child",
        type == "childToAdult" ~ "Child-to-adult",
        type == "childToChild" ~ "Child-to-child",
        type == "bothFemale" ~ "Female-to-female",
        type == "bothMale" ~ "Male-to-male",
        type == "femaleToMale" ~ "Female-to-male",
        type == "maleToFemale" ~ "Male-to-female",
        type == "donorOnly" ~ "Vaccinated-to-unvaccinated",
        type == "recipientOnly" ~ "Unvaccinated-to-vaccinated",
        type == "both" ~ "Vaccinated-to-vaccinated",
        type == "neither" ~ "Unvaccinated-to-unvaccinated",
        type == "overall" ~ "Overall",
        .default = type
      )
    )

  clonal_num_samples <- read_csv(pathToClonalNumSamples) %>%
    select(level, num) %>%
    dplyr::rename(type = level) %>%
    mutate(
      type = case_when(
        type == "Both Female" ~ "Female-to-female",
        type == "Both male" ~ "Male-to-male",
        type == "Donor only" ~ "Vaccinated-to-unvaccinated",
        type == "Recipient only" ~ "Unvaccinated-to-vaccinated",
        type == "Both" ~ "Vaccinated-to-vaccinated",
        type == "Neither" ~ "Unvaccinated-to-unvaccinated",
        type == "overall" ~ "Overall",
        .default = type
      )
    )

  df <- full_join(bottlenecks_by_factor, clonal_num_samples, by = "type") %>%
    mutate(
      cat = case_when(
        type == "17/18" |
          type == "18/19" | type == "19/20" | type == "21/22" ~ "Season",
        type == "Adult-to-adult" |
          type == "Adult-to-child" |
          type == "Child-to-child" | type == "Child-to-adult"  ~ "Age",
        type == "H1N1" | type == "H3N2" ~ "Subtype",
        type == "Male-to-male" |
          type == "Male-to-female" |
          type == "Female-to-male" |
          type == "Female-to-female" ~ "Sex-at-birth",
        type == "Vaccinated-to-vaccinated" |
          type == "Vaccinated-to-unvaccinated" |
          type == "Unvaccinated-to-vaccinated" |
          type == "Unvaccinated-to-unvaccinated" ~ "Vaccination",
        .default = "Overall"
      )
    ) %>%
    mutate(cat = factor(
      cat,
      levels = c(
        "Overall",
        "Season",
        "Subtype",
        "Age",
        "Sex-at-birth",
        "Vaccination"
      )
    )) %>%
    mutate(type2 = row_number())

  if (showMu == TRUE) {
    ggplot() +
      geom_col(df,
               mapping = aes(
                 x = type2,
                 y = 10 * meanNb,
                 fill = cat
               ),
               col = "black") +
      geom_col(df, mapping = aes(x = type2, y = -num), alpha = 0.25) +
      geom_point(
        df,
        mapping = aes(x = type2, y = 10 * mu),
        col = "red",
        size = 7
      ) +
      scale_y_continuous(
        breaks = c(-60, -40, -20, 10.04556),
        labels = c(60, 40, 20, 1.05),
        limits = c(-60, 60),
        sec.axis = sec_axis(
          ~ . * 1,
          breaks = c(10, 20, 30, 40, 50, 60),
          labels = c(1, 2, 3, 4, 5, 6)
        )
      ) +
      scale_fill_manual(values = c(
        "grey60",
        "#ff7e26",
        "#ffc99d",
        "#691883",
        "#b148d2",
        "#f3ccff"
      )) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        breaks = seq(1, 19, 1),
        labels = levels,
        expand = c(0, 0),
        limits = c(0.5, 18.5)
      ) +
      guides(shape = guide_legend(position = "top")) +
      geom_vline(xintercept = c(1.5, 5.5, 7.5, 11.5, 15.5)) +
      theme_bottleneck() +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.title = element_blank(),
      )
  } else {
    ggplot() +
      geom_col(df,
               mapping = aes(
                 x = type2,
                 y = 10 * meanNb,
                 fill = cat
               ),
               col = "black") +
      geom_col(df, mapping = aes(x = type2, y = -num), alpha = 0.25) +
      scale_y_continuous(
        breaks = c(-60, -40, -20, 10.04556),
        labels = c(60, 40, 20, 1.05),
        limits = c(-60, 60),
        sec.axis = sec_axis(
          ~ . * 1,
          breaks = c(10, 20, 30, 40, 50, 60),
          labels = c(1, 2, 3, 4, 5, 6)
        )
      ) +
      scale_fill_manual(values = c(
        "grey60",
        "#ff7e26",
        "#ffc99d",
        "#691883",
        "#b148d2",
        "#f3ccff"
      )) +
      geom_hline(yintercept = 0) +
      scale_x_continuous(
        breaks = seq(1, 19, 1),
        labels = levels,
        expand = c(0, 0),
        limits = c(0.5, 18.5)
      ) +
      guides(shape = guide_legend(position = "top")) +
      geom_vline(xintercept = c(1.5, 5.5, 7.5, 11.5, 15.5)) +
      theme_bottleneck() +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        axis.text.y.right = element_text(color = "white"),
        axis.ticks.y.right = element_blank(),
        axis.title = element_blank(),
      )
  }
}
