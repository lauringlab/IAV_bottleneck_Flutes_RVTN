makePairTable <- function() {
  isnv_pair <- read.table(
    "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt",
    header = TRUE
  ) %>%
    # Remove the pairs that were removed from the dataset due to no iSNVs
    filter(pair_id != 55 & pair_id != 62 & pair_id != 63)
  
  clonal_pair <- read.table(
    "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_dist_with_meta.txt",
    header = TRUE
  )
  
  list <- list()
  
  # Basic characteristics ------------------------------------------------------
  
  # a. number of samples
  isnv_samples <- isnv_pair %>%
    dplyr::select(donor_id, recipient_id, season, hhid) %>%
    gather(type, sample, -c(season, hhid)) %>%
    dplyr::select(-type) %>%
    distinct() %>%
    dplyr::count(name = "isnv") %>%
    mutate(metric = "Samples used")
  
  clonal_samples <- clonal_pair %>%
    dplyr::select(donor_id, recipient_id, season, hhid) %>%
    gather(type, sample, -c(season, hhid)) %>%
    dplyr::select(-type) %>%
    distinct() %>%
    dplyr::count(name = "clonal") %>%
    mutate(metric = "Samples used")
  
  list[[1]] <- full_join(isnv_samples, clonal_samples) %>%
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_samples)
  rm(clonal_samples)
  
  # b. number of households 
  isnv_households <- isnv_pair %>% 
    dplyr::select(hhid) %>% distinct() %>% 
    dplyr::count(name = "isnv") %>% 
    mutate(metric = "households")
  
  clonal_households <- clonal_pair %>% 
    dplyr::select(hhid) %>% distinct() %>% 
    dplyr::count(name = "clonal") %>% 
    mutate(metric = "households")
  
  list[[2]] <- full_join(isnv_households, clonal_households) %>% 
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_households)
  rm(clonal_households)
  
  #c. pairs
  list[[3]] <- tibble(
    metric = "pairs",
    isnv = nrow(isnv_pair),
    clonal = nrow(clonal_pair)
  )
  
  # Determine number of pairs for each cofactor ----------------------------------
  
  # a. Season
  isnv_season <- isnv_pair %>%
    group_by(season) %>%
    dplyr::count(name = "isnv") %>%
    dplyr::rename(metric = season)
  
  clonal_season <- clonal_pair %>%
    group_by(season) %>%
    dplyr::count(name = "clonal") %>%
    dplyr::rename(metric = season)
  
  list[[4]] <- full_join(isnv_season, clonal_season) %>%
    dplyr::select(metric, isnv, clonal) %>% 
    mutate(metric = as.character(metric))
  
  rm(isnv_season)
  rm(clonal_season)
  
  # b. Subtype
  isnv_subtype <- isnv_pair %>%
    mutate(subtype = str_extract(strain, "(?<=_)[A-Z0-9]+(?=_)")) %>%
    group_by(subtype) %>%
    dplyr::count(name = "isnv") %>%
    dplyr::rename(metric = subtype)
  
  clonal_subtype <- clonal_pair %>%
    mutate(subtype = str_extract(strain, "(?<=_)[A-Z0-9]+(?=_)")) %>%
    group_by(subtype) %>%
    dplyr::count(name = "clonal") %>%
    dplyr::rename(metric = subtype)
  
  list[[5]] <- full_join(isnv_subtype, clonal_subtype) %>%
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_subtype)
  rm(clonal_subtype)
  
  # c. vax
  isnv_vax <- isnv_pair %>%
    mutate(
      donor_vax = ifelse(
        is.na(donor_vax) |
          donor_vax == 0,
        "Unknown or unvaccinated",
        "Vaccinated"
      ),
      recipient_vax = ifelse(
        is.na(recipient_vax) |
          recipient_vax == 0,
        "Unknown or unvaccinated",
        "Vaccinated"
      )
    ) %>%
    dplyr::select(pair_id, donor_vax, recipient_vax) %>%
    mutate(metric = paste(donor_vax, "/", recipient_vax)) %>%
    group_by(metric) %>%
    dplyr::count(name = "isnv")
  
  clonal_vax <- clonal_pair %>%
    mutate(
      donor_vax = ifelse(
        is.na(donor_vax) |
          donor_vax == 0,
        "Unknown or unvaccinated",
        "Vaccinated"
      ),
      recipient_vax = ifelse(
        is.na(recipient_vax) |
          recipient_vax == 0,
        "Unknown or unvaccinated",
        "Vaccinated"
      )
    ) %>%
    dplyr::select(pair_id, donor_vax, recipient_vax) %>%
    mutate(metric = paste(donor_vax, "/", recipient_vax)) %>%
    group_by(metric) %>%
    dplyr::count(name = "clonal")
  
  list[[6]] <- full_join(isnv_vax, clonal_vax) %>%
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_vax)
  rm(clonal_vax)
  
  # d. sex
  isnv_sex <- isnv_pair %>%
    mutate(
      donor_sex = ifelse(donor_sex == 1, "Female", "Male"),
      recipient_sex = ifelse(recipient_sex == 1, "Female", "Male")
    ) %>%
    dplyr::select(pair_id, donor_sex, recipient_sex) %>%
    mutate(metric = paste(donor_sex, "/", recipient_sex)) %>%
    group_by(metric) %>%
    dplyr::count(name = "isnv")
  
  clonal_sex <- clonal_pair %>%
    mutate(
      donor_sex = ifelse(donor_sex == 1, "Female", "Male"),
      recipient_sex = ifelse(recipient_sex == 1, "Female", "Male")
    ) %>%
    dplyr::select(pair_id, donor_sex, recipient_sex) %>%
    mutate(metric = paste(donor_sex, "/", recipient_sex)) %>%
    group_by(metric) %>%
    dplyr::count(name = "clonal")
  
  list[[7]] <- full_join(isnv_sex, clonal_sex) %>%
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_sex)
  rm(clonal_sex)
  
  # 4. age
  
  isnv_age <- isnv_pair %>%
    mutate(
      donor_age = ifelse(donor_age < 18, "Child", "Adult"),
      recipient_age = ifelse(recipient_age < 18, "Child", "Adult")
    ) %>%
    dplyr::select(pair_id, donor_age, recipient_age) %>%
    mutate(metric = paste(donor_age, "/", recipient_age)) %>%
    group_by(metric) %>%
    dplyr::count(name = "isnv")
  
  clonal_age <- clonal_pair %>%
    mutate(
      donor_age = ifelse(donor_age < 18, "Child", "Adult"),
      recipient_age = ifelse(recipient_age < 18, "Child", "Adult")
    ) %>%
    dplyr::select(pair_id, donor_age, recipient_age) %>%
    mutate(metric = paste(donor_age, "/", recipient_age)) %>%
    group_by(metric) %>%
    dplyr::count(name = "clonal")
  
  list[[8]] <- full_join(isnv_age, clonal_age) %>%
    dplyr::select(metric, isnv, clonal)
  
  rm(isnv_age)
  rm(clonal_age)
  
  out <- bind_rows(list)
  
  return(out)
  
}
