## Title: Other Descriptive Plots
## Author: Katy Krupinsky
## Date Last Modified: 02/23/23
## Description: This script generates the iSNV frequency plots and the TV plot.
##
## -----------------------------------------------------------------------------

pair_meta <- read.table("03 - Input/pair_meta.txt", header = TRUE)
pair_snv <- read.table("03 - Input/pairsnv.txt", header = TRUE)
ind_snv <- read.table("03 - Input/indsnv.txt", header = TRUE)
pair_snv_tv <- read.table("03 - Input/pairsnv_tv.txt", header = TRUE)
pair <- full_join(pair_meta, pair_snv, relationship = "one-to-many")
pair_tv <- full_join(pair_meta, pair_snv_tv, relationship = "one-to-many")

## iSNVs per pair --------------------------------------------------------------
temp <- df_tv %>%
  ungroup() %>%
  group_by(season, pair_id, strain) %>%
  count() %>%
  mutate(pair_id = as.numeric(pair_id)) %>%
  arrange(pair_id)

temp2 <- temp %>%
  ungroup() %>%
  group_by(season, strain) %>%
  count()

temp3 <- temp %>%
  ungroup() %>%
  group_by(n) %>%
  count() %>%
  dplyr::rename(frequency = nn) %>%
  full_join(temp)

temp3 %>%
  ggplot() +
  geom_density(aes(x = n, fill = strain), alpha = 0.5) +
  geom_rug(aes(x = n, col = frequency), alpha = 0.3) +
  facet_wrap(vars(season)) +
  theme_light(base_size = 18)

## TV plot - faceted -----------------------------------------------------------
colors <- c(
  "#4E79A7",
  "#F28E2B",
  "#A0CBE8",
  "#A0CBE8",
  "#FFBE7D",
  "#59A14F",
  "#8CD17D",
  "#B6992D",
  "#F1CE63",
  "#499894",
  "#499894",
  "#86BCB6",
  "#E15759",
  "#FF9D9A",
  "#79706E",
  "#BAB0AC",
  "#D37295",
  "#FABFD2",
  "#B07AA1",
  "#D4A6C8",
  "#9D7660",
  "#D7B5A6"
)

pairs_info <- pair_meta %>%
  arrange(pair_id) %>%
  mutate(pair_alpha = LETTERS[1:22], colors = colors)

pairs <- pairs_info %>%
  select(pair_id) %>%
  as_vector()

plot_list <- list()

for (i in pairs) {
  df_facet <- df_tv %>%
    filter(pair_id == i) %>%
    mutate(non_fixed = ifelse(non_fixed == 0, NA, non_fixed))
  plot_color <- pairs_info$colors[i]
  plot_title <- pairs_info$pair_alpha[i]
  plot <- df_facet %>%
    ggplot(aes(x = donor_freq2, y = recipient_freq2)) +
    geom_point(
      aes(color = as.factor(non_fixed), alpha = as.factor(non_fixed)),
      size = 3,
      shape = 20,
      show.legend = FALSE
    ) +
    theme_classic(base_size = 7) +
    geom_rect(aes(
      xmin = 0,
      xmax = 1,
      ymin = 1.05,
      ymax = 1.2
    ), fill = plot_color) +
    geom_vline(xintercept = c(0.005, 0.995)) +
    geom_hline(yintercept = c(0.005, 0.995)) +
    ylim(c(0, 1.2)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0, 0.05), breaks = seq(0, 1, by = 0.25)) +
    xlab(pairs_info$donor[i]) +
    ylab(pairs_info$recipient[i]) +
    scale_color_manual(values = c("black"), na.value = "red") +
    scale_alpha_manual(values = c(0.5), na.value = 1) +
    theme(
      plot.margin = margin(1, 1, 1.5, 1.2, "cm"),
      axis.text.x = element_text(
        angle = 0,
        vjust = 0.3,
        hjust = 0.3
      )
    ) +
    coord_capped_cart(left = 'top') +
    annotate(
      'text',
      x = 0.50,
      y = 1.13,
      label = plot_title,
      size = 3,
      fontface = "bold",
      color = "white"
    ) +
    theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
  
  plot_list[[i]] <- plot
}

tv_plot_facet <- cowplot::plot_grid(
  plot_list[[1]],
  plot_list[[2]],
  plot_list[[3]],
  plot_list[[4]],
  plot_list[[5]],
  plot_list[[6]],
  plot_list[[7]],
  plot_list[[8]],
  plot_list[[9]],
  plot_list[[10]],
  plot_list[[11]],
  plot_list[[12]],
  plot_list[[13]],
  plot_list[[14]],
  plot_list[[15]],
  plot_list[[16]],
  plot_list[[17]],
  plot_list[[18]],
  plot_list[[19]],
  plot_list[[20]],
  plot_list[[21]],
  plot_list[[22]],
  ncol = 4
) +
  theme(plot.margin = margin(1, 1, 1.5, 1.2, "cm"))
