plotSnvByCt <- function(df) {
  df2 <- df %>%
    group_by(sample, subtype_ct) %>%
    dplyr::count()
  
  
  df2 %>%
    ggplot(aes(x = subtype_ct, y = n)) +
    geom_point() +
    stat_poly_eq(mapping = use_label("R2", "eq"),
                 size = 5,
                 label.x = 0.93) +
    stat_poly_line(se = FALSE, color = "#2c6184") +
    labs(x = "Ct value", y = "Number of iSNV") +
    theme_minimal(base_size = 15)
}