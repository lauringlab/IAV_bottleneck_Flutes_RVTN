makeTree <- function(pathToMeta,
                     strain_int = "Mighigan_H1N1_2017",
                     pathToTreeFile,
                     pathToPair,
                     scale_pos,
                     offset = 0.28,
                     fontsize = 6) {
  meta <- read.table(pathToMeta, header = TRUE) %>%
    dplyr::rename(label = sample) %>%
    dplyr::filter(strain == strain_int) %>%
    dplyr::select(label, hhid) %>%
    dplyr::mutate(label = as.character(label)) %>%
    distinct()
  
  pair <- read.table(pathToPair, header = TRUE) %>%
    dplyr::filter(strain == strain_int) %>%
    dplyr::mutate(donor_id = as.character(donor_id),
                  recipient_id = as.character(recipient_id))
  
  file <- read.tree(pathToTreeFile)
  
  out <- full_join(file, meta, by = "label")
  
  ggtree(out, size = 0.7) +
    geom_tippoint(
      out,
      mapping =
        aes(fill = as.factor(hhid)),
      size = 5,
      shape = 21,
      col = "black",
      show.legend = FALSE
    ) +
    geom_cladelabel(node = 1, label = "Ref", offset = 0.00002, fontsize = fontsize) +
    scale_fill_paletteer_d("ggthemes::Tableau_20", na.value = "white") +
    scale_color_paletteer_d("ggthemes::Tableau_20") +
    geom_treescale(
      width = 0.001,
      x = 0,
      y = scale_pos,
      offset = offset,
      fontsize = fontsize
    ) +
    theme(plot.margin = margin(t = 1, b = 1, l = 1, r = 1, unit = "cm"))
  
}