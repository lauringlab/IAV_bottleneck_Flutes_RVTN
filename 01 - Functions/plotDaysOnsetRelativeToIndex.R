plotDaysOnsetRelativeToIndex <- function(df) {
  d <- df %>%
    select(season,
           pair_id,
           hhid,
           donor_id,
           recipient_id,
           donor_onset,
           recipient_onset) %>%
    mutate(
      donor_onset = as_date(donor_onset),
      recipient_onset = as_date(recipient_onset),
      days_between = recipient_onset - donor_onset,
      days_between = as.numeric(days_between)
    ) %>%
    select(-c(donor_onset, recipient_onset))
  
  households <- d %>% select(hhid) %>% distinct() %>% as_vector()
  
  list <- list()
  
  for (i in 1:length(households)) {
    h <- households[[i]]
    
    d2 <- d %>%
      filter(hhid == h)
    
    
    if (nrow(d2) == 1) {
      new <- d2 %>%
        mutate(donor_pos = 2, recipient_pos = 2)
    } else if (nrow(d2) == 2) {
      if (length(unique(d2$donor_id) == 1)) {
        new <- d2 %>%
          mutate(donor_pos = 2, recipient_pos = c(1, 3))
      } else {
        print(i)
        print("New case")
      }
    } else if (nrow(d2) == 3) {
      if (length(unique(d2$donor_id)) == 1) {
        new <- d2 %>%
          mutate(donor_pos = 2,
                 recipient_pos = c(1, 2, 3))
      }
    } else if (nrow(d2) == 4) {
      if (length(unique(d2$donor_id)) == 2) {
        loc <- d2 %>%
          select(donor_id) %>% distinct() %>% mutate(loc = c(1, 3))
        new <- d2 %>%
          mutate(
            donor_pos = case_when(
              donor_id == loc$donor_id[[1]] ~ loc$loc[[1]],
              donor_id == loc$donor_id[[2]] ~ loc$loc[[2]],
              .default = 999
            ),
            recipient_pos = case_when(
              recipient_id == loc$donor_id[[1]] ~ loc$loc[[1]],
              recipient_id == loc$donor_id[[2]] ~ loc$loc[[2]],
              .default = 2
            )
          )
      }
    } else if (nrow(d2 == 6)) {
      if (sum(d2$days_between) == 0) {
        loc <- d2 %>%
          select(donor_id) %>% distinct() %>% mutate(loc = row_number())
        new <- d2 %>%
          mutate(
            donor_pos = case_when(
              donor_id == loc$donor_id[[1]] ~ loc$loc[[1]],
              donor_id == loc$donor_id[[2]] ~ loc$loc[[2]],
              donor_id == loc$donor_id[[3]] ~ loc$loc[[3]],
              .default = 999
            ),
            recipient_pos = case_when(
              recipient_id == loc$donor_id[[1]] ~ loc$loc[[1]],
              recipient_id == loc$donor_id[[2]] ~ loc$loc[[2]],
              recipient_id == loc$donor_id[[3]] ~ loc$loc[[3]],
              .default = 999
            )
          )
      } else {
        print(i)
        print("New case")
      }
    } else {
      print(i)
      print("New case")
    }
    
    list[[i]] <- new
  }
  
  d3 <- bind_rows(list)
  
  house <- tibble(hhid = households) %>%
    dplyr::mutate(house = row_number())
  
  d4 <- d3 %>%
    full_join(house) %>%
    mutate(
      donor_pos_new = case_when(
        donor_pos == 1 ~ house - 0.5,
        donor_pos == 2 ~ house,
        donor_pos == 3 ~ house + 0.5,
        .default = 999
      ),
      recipient_pos_new = case_when(
        recipient_pos == 1 ~ house - 0.5,
        recipient_pos == 2 ~ house,
        recipient_pos == 3 ~ house + 0.5,
        .default = 999
      )
    )
  
  
  
  ggplot() +
    geom_segment(
      d4,
      mapping = aes(
        x = 0,
        xend = days_between,
        y = donor_pos_new,
        yend = recipient_pos_new,
        col = as.factor(season)
      ),
      col = "#89689d",
      linewidth = 1
    ) +
    geom_point(d4,
               mapping = aes(x = days_between, y = recipient_pos_new),
               size = 2) +
    geom_point(d4,
               mapping = aes(x = 0, y = donor_pos_new),
               size = 2) +
    geom_segment(
      aes(
        x = -0.35,
        xend = -0.35,
        y = c(0.8, 2, 20, 31),
        yend = c(1.2, 19, 30, 47)
      ),
      linewidth = 1,
      col = "grey50"
    ) +
    theme_classic(base_size = 15) +
    scale_x_continuous(
      breaks = seq(0, 10, 1),
      limits = c(-0.38, 10.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = c(0.01, 0.01),
      breaks = c(1, 10.5, 25, 39),
      labels = c("2017-2018", "2018-2019", "2019-2020", "2021-2022")
    ) +
    labs(x = "Day of onset relative to index case") +
    theme(
      axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank()
    )
  
}