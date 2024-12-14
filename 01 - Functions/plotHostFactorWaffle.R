plotHostFactorWaffle <- function(df = bottleneck_meta2,
                                 cat_var = "age_cat",
                                 cols = c("darkslategrey",
                                          "darkslategray4",
                                          "darkslategray3",
                                          "darkslategray1"),
                                 cat = "ages") {
  t <- df %>%
    select(cat_var) %>%
    dplyr::rename(var = cat_var) %>%
    group_by(var) %>%
    count() %>%
    arrange(desc(cat_var)) %>%
    mutate(cat_var = as_factor(cat_var))
  
  t %>%
    ggplot() +
    geom_waffle(aes(fill = var, values = n),
                size = 2,
                col = "black",
                n_rows = 17) +
    scale_fill_manual(values = cols) +
    theme_void(base_size = 23) +
    labs(fill = element_blank()) +
    guides(fill = guide_legend(ncol = 2)) +
    ggtitle(cat) +
    theme(
      plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = 0),
      legend.position = "bottom",
      plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
      legend.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}