library(ggridges)

plotSerialSubfactors <- function(pathToPairMeta = './03 - Input/pair_meta.txt') {
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
  
  df2 <- df %>%
    mutate(
      subtype = str_extract(strain, "H\\d+N\\d+"),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22"
      )
    ) %>%
    mutate(age_diff = donor_age - recipient_age) %>%
    mutate(
      age_cat = case_when(
        donor_age < 18 & recipient_age < 18 ~ "Child-to-child",
        donor_age < 18 & recipient_age >= 18 ~ "Child-to-adult",
        donor_age >= 18 & recipient_age >= 18 ~ "Adult-to-adult",
        donor_age >= 18 & recipient_age < 18 ~ "Adult-to-child"
      ),
      vax_cat = case_when(
        donor_vax == 0 & recipient_vax == 0 ~ "Neither",
        donor_vax == 0 &
          recipient_vax == 1 ~ "Recipient only",
        donor_vax == 1 & recipient_vax == 0 ~ "Donor only",
        donor_vax == 1 & recipient_vax == 1 ~ "Both"
      ),
      sex_cat = case_when(
        donor_sex == 1 & recipient_sex == 1 ~ "Both Female",
        donor_sex == 1 & recipient_sex == 2 ~ "Female-to-male",
        donor_sex == 2 & recipient_sex == 1 ~ "Male-to-female",
        donor_sex == 2 & recipient_sex == 2 ~ "Both male"
      )
    )
  
  df3 <- df2 %>%
    select(pair_serial, age_cat, vax_cat, sex_cat, subtype) %>%
    gather(type, level, -c(pair_serial)) %>%
    mutate(
      type_l = case_when(
        type == "subtype" ~ 1,
        type == "age_cat" ~ 2,
        type == "sex_cat" ~ 3,
        type == "vax_cat" ~ 4
      )
    ) %>%
    arrange(type_l, level) %>%
    mutate(level = as_factor(level))
  
  df3 %>%
    ggplot(aes(x = pair_serial, y = level, fill = type)) +
    ggridges::geom_density_ridges(alpha = 0.5) +
    theme_bw(base_size = 24) +
    labs(x = "Serial Interval", y = "") +
    scale_fill_manual(values = c("#691883", "#b148d2", "#ffc99d","#f3ccff")) +
    theme(legend.position = "none")
}