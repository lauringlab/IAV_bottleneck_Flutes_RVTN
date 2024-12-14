getRvtnMetadata <- function(path_to_fulldat, path_to_ddlabdat) {
  # 1. Import the individual datasets
  fulldat_raw <- read_csv(path_to_fulldat)
  ddlabdat_raw <- read_csv(path_to_ddlabdat)
  
  # 2. Determine which ones are RVTN and which ones are FLUTES -------------------
  fulldat_1 <- fulldat_raw %>%
    select(cdc_studyid,
           cdc_hhid,
           cdc_site,
           season,
           age_at_enrollment,
           sex,
           cdc_flu_vx,
           cdc_symptom_startdt,
           hh_number,
           ever_antiviral) %>% 
    mutate(rvtn = ifelse(is.na(ever_antiviral), 1, 0),
           flutes = ifelse(rvtn == 1, 0, 1)) %>%
    select(-ever_antiviral)
  
  # 3. Pull out the variables which we actually need -----------------------------
  ddlabdat_1 <- ddlabdat_raw %>%
    select(cdc_studyid,
           cdc_specid,
           date)
  
  # 4. Put the different meta - data sets together and make the columns in a way
  # which doesn't drive Katy crazy ---------------------------------------------
  meta <- full_join(fulldat_1, ddlabdat_1, multiple = "all") %>%
    dplyr::rename(
      vax = cdc_flu_vx,
      onset_date = cdc_symptom_startdt
    ) %>% 
    filter(rvtn == 1)
  
  return(meta)
}
