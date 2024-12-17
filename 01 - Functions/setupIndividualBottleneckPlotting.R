setupIndividualBottleneckPlotting <- function(pathToPairMeta = "03 - Input/pair_meta.txt",
                                              pathToPairSnv = "03 - Input/pairsnv.txt",
                                              pathToConfidenceInt = "04 - Output/confidence_int.txt",
                                              pathToLogLikelihood = "04 - Output/logLikelihood.txt",
                                              pathToNumVars = "04 - Output/num_vars.txt") {
  ### Data frame setup ###
  pair_meta <- read.table(pathToPairMeta, header = TRUE)
  pair_snv <- read.table(pathToPairSnv, header = TRUE)
  pair <- full_join(pair_meta, pair_snv, relationship = "one-to-many")
  confidence_int <- read.table(pathToConfidenceInt, header = TRUE)
  logLikelihood <- read.table(pathToLogLikelihood, header = TRUE)
  num_vars <- read.table(pathToNumVars, header = TRUE)
  bottleneck <- confidence_int %>% full_join(num_vars) %>% dplyr::rename(pair_id = pair)
  bottleneck_meta <- bottleneck %>% full_join(pair_meta)
  df <- bottleneck %>% full_join(pair)
  
  bottleneck_meta2 <- bottleneck_meta %>%
    mutate(
      subtype = str_extract(strain, "H\\d+N\\d+"),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22"
      )
    ) %>%
    filter(max_LL != 999) %>%
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
}