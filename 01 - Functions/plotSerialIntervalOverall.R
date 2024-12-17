library(ggbeeswarm)
library(tidyverse)

plotSerialIntervalOverall <- function(pathToPairMeta = './03 - Input/pair_meta.txt') {
  df <- read.table(pathToPairMeta, header = TRUE) %>%
    ungroup() %>%
    mutate(
      donor_collection = as_date(donor_collection),
      recipient_collection = as_date(recipient_collection),
      donor_onset = as_date(donor_onset),
      recipient_onset = as_date(recipient_onset)
    ) %>%
    mutate(
      donor_delay_collection = as.numeric(donor_collection - donor_onset),
      recipient_delay_collection = as.numeric(recipient_collection - recipient_onset),
      pair_serial = recipient_onset - donor_onset
    )
  
  df %>%
    ggplot(aes(x = as.factor(season), y = pair_serial)) +
    geom_boxplot(outlier.shape = NA) +
    geom_beeswarm(method = "center", size = 2) +
    theme_bw(base_size = 24) +
    labs(x = "Season", y = "Serial Interval", col = "Pair Role") +
    scale_y_continuous(minor_breaks = seq(-1, 15, 1))
}
