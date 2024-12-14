plotMetafactorWaffle <- function(bottleneck_meta2) {
  metafactor_plot_age <- bottleneck_meta2 %>%
    group_by(age_cat) %>%
    dplyr::rename(cat = age_cat) %>%
    count() %>%
    ungroup() %>%
    mutate(factor = "age")
  
  metafactor_plot_sex <- bottleneck_meta2 %>%
    group_by(sex_cat) %>%
    dplyr::rename(cat = sex_cat) %>%
    count() %>%
    ungroup() %>%
    mutate(factor = "sex")
  
  metafactor_plot_vax <- bottleneck_meta2 %>%
    group_by(vax_cat) %>%
    dplyr::rename(cat = vax_cat) %>%
    count() %>%
    ungroup() %>%
    mutate(factor = "vax")
  
  metafactor_plot <- metafactor_plot_vax %>%
    bind_rows(metafactor_plot_age) %>%
    bind_rows(metafactor_plot_sex)
  
  levels <- c(
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
  
  metafactor_plot %>%
    ggplot() +
    geom_waffle(
      mapping = aes(values = n, fill = factor(cat, levels = levels)),
      col = "black",
      size = 2
    ) +
    facet_wrap(vars(factor)) +
    theme_void(base_size = 20) +
    scale_fill_paletteer_d("ggthemes::manyeys", direction = -1) +
    guides(fill = guide_legend(nrow = 2, position = "bottom")) +
    theme(
      legend.text = element_text(face = "bold"),
      legend.title = element_blank(),
      strip.text = element_blank(),
      plot.margin = margin(1, 1, 1.5, 1.2, "cm")
    )
}
