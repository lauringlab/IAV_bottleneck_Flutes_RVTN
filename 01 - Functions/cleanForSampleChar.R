cleanForSampleChar <- function(df) {
  df2 <- df %>% 
    select(cdc_studyid, cdc_hhid, cdc_site, season, index, age_at_enrollment,
           sex, season_vax, symptomatic, symptom_start, cdc_flu_type2,
           persons_per_household) %>% 
    distinct() %>% 
    mutate(sex = ifelse(sex == 1, "Female", "Male"),
           symptomatic = ifelse(symptomatic == 1, "Symptomatic", "Asymptomatic"),
           season = ifelse(season == 1718, "17/18",
                           ifelse(season == 1819, "18/19",
                                  ifelse(season == 1920, "19/20",
                                         ifelse(season == 2122, "21/22", "22/23")))),
           age_d = ifelse(age_at_enrollment < 18, "Child", "Adult")) %>% 
    filter(cdc_flu_type2 == "A_H1" | cdc_flu_type2 == "A_H3")
  
  return(df2)
}