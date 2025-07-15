plotDaysPostSymptomOnset <- function(df){
  t <- df %>% 
    dplyr::select(pair_id, donor_onset, recipient_onset, donor_collection, recipient_collection) %>% 
    dplyr::mutate(donor_onset = as_date(donor_onset),
                  recipient_onset = as_date(recipient_onset),
                  donor_collection = as_date(donor_collection),
                  recipient_collection = as_date(recipient_collection)) %>% 
    dplyr::mutate(donor = donor_collection - donor_onset,
                  recipient = recipient_collection - recipient_onset) %>% 
    dplyr::select(pair_id, donor, recipient) %>% 
    dplyr::mutate(pair_id = as.factor(pair_id),
                  pair_id = fct_reorder(pair_id, recipient),
                  same = if_else(donor == recipient, TRUE, FALSE)) 
  
  ggplot() +
    geom_segment(
      data = t,
      mapping = aes(
        x = donor,
        xend = recipient,
        y = pair_id,
        yend = pair_id
      )
    ) +
    geom_point(
      data = t %>% dplyr::filter(same == FALSE),
      mapping = aes(x = donor, y = pair_id, col = "Donor"),
      size = 2
    ) +
    geom_point(
      data = t %>% dplyr::filter(same == FALSE),
      mapping = aes(x = recipient, y = pair_id, col = "Recipient"),
      size = 2
    ) +
    geom_point(
      data = t %>% dplyr::filter(same == TRUE),
      mapping = aes(x = donor, y = pair_id, col = "Donor and recipient"),
      size = 2
    ) +
    scale_x_continuous(breaks = seq(-4, 12, 2)) +
    scale_color_manual(values = c("#e69b99", "#89689d", "#015b58")) +
    labs(x = "Days post symptom onset") +
    theme_classic(base_size = 15) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.085, 0.92),
      legend.background = element_rect(color = "black", linewidth = 0.5)
    )
}