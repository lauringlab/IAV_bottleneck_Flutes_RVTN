joinFlutesSnvToMeta <- function(path_to_flutes_snv,
                                path_to_flutes_meta,
                                path_to_ddlabdat,
                                path_to_fulldat,
                                ctThresh = 30,
                                sequenced_ids_df = sequenced_ids_df) {
  # 1. Format metadata (Lauring Lab) -------------------------------------------
  flutes_meta <- read.csv(path_to_flutes_meta)
  
  flutes_meta <- flutes_meta %>%
    dplyr::rename(
      sample = Study_ID,
      onset_date = onsetdt,
      collection_date = SPECDT_1,
      age = age.new,
      season = Year,
      vax = p_seasvx
    ) %>%
    mutate(
      collection_date = ifelse(collection_date == ".", NA, collection_date),
      onset_date = ifelse(onset_date == ".", NA, onset_date),
      collection_date = mdy(collection_date),
      onset_date = mdy(onset_date),
      sex = ifelse(sex == 0, 1, 2)
    ) %>%
    select(sample,
           hhsubid,
           hhid,
           season,
           age,
           sex,
           vax,
           collection_date,
           onset_date)
  
  # 2. Format metadata (CDC) ---------------------------------------------------
  fulldat <- read.csv(path_to_fulldat) %>%
    select(cdc_studyid,
           cdc_hhid,
           cdc_site,
           season,
           age_at_enrollment,
           cdc_symptom_startdt,
           sex,
           cdc_flu_vx)
  
  ddlabdat <- read.csv(path_to_ddlabdat) %>%
    select(cdc_studyid, speccoll_1, date, cdc_flupos_sample)
  
  temp <- ddlabdat %>%
    ungroup() %>% 
    distinct() %>% 
    group_by(cdc_studyid, date, speccoll_1) %>% 
    mutate(count = row_number()) %>% 
    filter(count == 2)
  
  cdc_meta <- right_join(fulldat, ddlabdat) %>%
    filter(season %in% c(1718, 1819, 1920)) %>% 
    filter(cdc_site != "marshfield") %>%
    group_by(cdc_studyid) %>%
    mutate(
      specimen_number = row_number(),
      specimen_number = paste(0, specimen_number, sep = ""),
      season2 = str_sub(cdc_studyid, 2, 3),
      household = str_sub(cdc_studyid, 5, 7),
      member = str_sub(cdc_studyid, 9, 10),
      season = ifelse(season == 1718, 17, ifelse(season == 1819, 18, 19)),
      cdc_flu_vx = ifelse(cdc_flu_vx == "Unvaccinated", 0, 1)
    ) %>%
    mutate(sample = paste(season, household, member, speccoll_1, specimen_number, sep = "")) %>%
    dplyr::rename(age = age_at_enrollment,
                  vax = cdc_flu_vx,
                  collection_date = date,
                  onset_date = cdc_symptom_startdt) %>%
    ungroup() %>%
    mutate(hhsubid = str_sub(sample, 1, 7),
           hhid = str_sub(sample, 1, 5)) %>%
    select(sample, hhsubid, hhid, season, age, sex, vax, collection_date, onset_date) %>%
    mutate(
      sample = as.numeric(sample),
      collection_date = as.Date(collection_date),
      hhsubid = as.integer(hhsubid),
      hhid = as.integer(hhid),
      onset_date = as.Date(onset_date),
      cdc = TRUE
    ) %>% 
    distinct() %>% 
    # select(-collection_date) %>% 
    ungroup()
  
  # 3. Format snv data ---------------------------------------------------------
  flutes_snv <- read.csv(path_to_flutes_snv)
  
  flutes_snv <- flutes_snv %>%
    # filter(subtype_ct < ctThresh) %>%
    dplyr::rename(
      age = age.new,
      season = Year,
      region = REGION,
      pos = POS,
      ref = REF,
      alt = ALT,
      onset_date = onsetdt,
      collection_date = SPECDT_1
    ) %>%
    mutate(
      collection_date = ifelse(collection_date == ".", NA, collection_date),
      onset_date = ifelse(onset_date == ".", NA, onset_date),
      sex = ifelse(sex == 0, 1, 2),
      collection_date = mdy(collection_date),
      onset_date = mdy(onset_date)
    ) %>%
    select(
      sample,
      onset_date,
      collection_date,
      age,
      sex,
      season,
      reference,
      avg_freq,
      subtype_ct,
      region,
      pos,
      ref,
      alt,
      avg_freq
    )
  
  # 3. Join metadata and snv data ----------------------------------------------
  flutes_unified <- flutes_snv %>%
    full_join(flutes_meta) %>% 
    mutate(sample = as.numeric(sample),
           season = as.numeric(season),
           sex = as.integer(sex)) %>% 
    # select(-vax) %>% 
    as_tibble() %>% 
    ungroup()
  
  flutes_unified2 <- flutes_unified %>%
    full_join(cdc_meta, by = c("sample", "hhsubid", "hhid", "season", "age", "sex", "onset_date")) %>% 
    mutate(site = "nashville") %>% 
    dplyr::rename(collection_date_lauring = collection_date.x,
                  collection_date_cdc = collection_date.y,
                  vax_lauring = vax.x,
                  vax_cdc = vax.y) %>% 
    filter(collection_date_lauring != collection_date_cdc | vax_lauring != vax_cdc) %>% 
    select(sample, collection_date_lauring, collection_date_cdc, vax_lauring, vax_cdc) %>% 
    distinct() %>%
    select(
      sample,
      cdc,
      hhsubid,
      hhid,
      site,
      season,
      age,
      sex,
      # vax,
      collection_date,
      onset_date,
      region,
      pos,
      ref,
      alt,
      avg_freq
    )
  
  return(flutes_unified2)
}