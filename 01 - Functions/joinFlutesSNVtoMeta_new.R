joinFlutesSnvToMeta_new <- function(path_to_flutes_snv,
                                path_to_flutes_meta,
                                path_to_ddlabdat,
                                path_to_fulldat,
                                pathTo1718SpecimenKeyForSequenced,
                                pathTo19SpecimenKeyForSequenced,
                                ctThresh = 30,
                                sequenced_ids_df = sequenced_ids_df) {
  # 1. Determine the specimenID for each of the samples ------------------------
  
  flutes_sequenced_ids <- sequenced_ids_df %>%
    filter(str_detect(as.character(sample), "^17|^18|^19"))
  
  flutes_19_sequenced_meta <- read_csv(pathTo19SpecimenKeyForSequenced) %>%
    dplyr::rename(sample = CaseID, specimen_id = SPECID_1) %>%
    select(sample, specimen_id)
  
  flutes_1718_sequenced_meta <- read_csv(pathTo1718SpecimenKeyForSequenced) %>%
    dplyr::rename(sample = CaseID, specimen_id = SPECID_1) %>%
    select(sample, specimen_id)
  
  flutes_sequenced_meta <- bind_rows(flutes_1718_sequenced_meta, flutes_19_sequenced_meta) %>%
    mutate(has_specimen_id = TRUE)
  
  test <- full_join(sequenced_ids_df, flutes_sequenced_meta) %>%
    filter(str_detect(as.character(sample), "^17|^18|^19")) %>%
    filter(sequenced == TRUE)
  
  # 1. Format metadata (Lauring Lab) -------------------------------------------
  flutes_meta <- read.csv(path_to_flutes_meta)
  
  flutes_meta2 <- flutes_meta %>%
    dplyr::rename(
      sample = Study_ID,
      onset_date = onsetdt,
      specimen_id = SPECID_1,
      collection_date = SPECDT_1,
      age = age.new,
      season = Year,
      vax = p_seasvx
    ) %>%
    mutate(
      onset_date = ifelse(onset_date == ".", NA, onset_date),
      collection_date = mdy(collection_date),
      onset_date = mdy(onset_date),
      sex = ifelse(sex == 0, 1, 2)
    ) %>%
    select(specimen_id,
           sample,
           onset_date,
           specimen_id,
           collection_date,
           age,
           season,
           vax,
           sex)
  
  # 2. Format snv data ---------------------------------------------------------
  flutes_snv <- read.csv(path_to_flutes_snv)
  
  flutes_snv2 <- flutes_snv %>%
    dplyr::rename(
      age = age.new,
      season = Year,
      region = REGION,
      pos = POS,
      ref = REF,
      alt = ALT,
      onset_date = onsetdt,
      collection_date = SPECDT_1,
      specimen_id = SPECID_1
    ) %>%
    mutate(
      onset_date = ifelse(onset_date == ".", NA, onset_date),
      collection_date = mdy(collection_date),
      onset_date = mdy(onset_date),
      sex = ifelse(sex == 0, 1, 2)
    ) %>%
    select(
      specimen_id,
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
  
  # 2. Format metadata (CDC) ---------------------------------------------------
  fulldat <- read.csv(path_to_fulldat)
  
  fulldat2 <- fulldat %>%
    select(
      cdc_studyid,
      cdc_hhid,
      cdc_site,
      season,
      age_at_enrollment,
      cdc_symptom_startdt,
      sex,
      cdc_flu_vx
    )
  
  ddlabdat <- read.csv(path_to_ddlabdat)
  
  ddlabdat2 <- ddlabdat %>%
    select(cdc_studyid, cdc_specid, speccoll_1, date, cdc_flupos_sample)
  
  cdc_meta <- full_join(fulldat2, ddlabdat2) %>%
    filter(season %in% c(1718, 1819, 1920)) %>%
    filter(cdc_site != "marshfield") %>%
    dplyr::rename(
      age = age_at_enrollment,
      vax = cdc_flu_vx,
      collection_date = date,
      onset_date = cdc_symptom_startdt,
      specimen_id = cdc_specid
    )  %>%
    mutate(
      vax = ifelse(vax == "Unvaccinated", 0, 1),
      season = as.numeric(str_sub(as.character(season), 1, 2)),
      age = floor(age * 100) / 100
    ) %>%
    select(
      specimen_id,
      cdc_studyid,
      cdc_hhid,
      cdc_site,
      season,
      age,
      sex,
      vax,
      onset_date,
      collection_date
    ) %>% 
    dplyr::rename(site = cdc_site)
  
  # 3. Join metadata and snv data ----------------------------------------------
  flutes_unified <- flutes_snv2 %>%
    full_join(
      flutes_meta2,
      by = c(
        "specimen_id",
        "sample",
        "age",
        "sex",
        "season",
        "onset_date",
        "collection_date"
      )
    ) %>%
    mutate(age = floor(age * 100) / 100)
  
  ## .x is from snv dataset, .y is from CDC dataset
  flutes_unified2 <- flutes_unified %>%
    full_join(cdc_meta, by = c("age", "sex", "season", "specimen_id"))
  
  ids_with_isnv <- flutes_snv2 %>% 
    select(specimen_id) %>% 
    distinct() %>% 
    mutate(isnv = TRUE)
    
  trial <- test %>% 
    left_join(ids_with_isnv)
  
  
  # 4. Exploratory work to figure out where there are disagreements ------------
  ### Emily B. decision: defer to CDC when there are disagreements
  flutes_unified_sequenced <- flutes_unified2 %>%
    ## Only keep those that are sequenced
    full_join(test, by = c("specimen_id")) %>%
    filter(sequenced == TRUE) %>%
    ## Input the sample ID for those that it is missing
    mutate(sample = ifelse(is.na(sample.x), sample.y, sample.x)) %>%
    select(-c(sample.x, sample.y)) %>%
    ## Determine those where the collection date or onset date are different between the two datasets
    mutate(
      collection_date.y = as_date(collection_date.y),
      collection_date.x = as_date(collection_date.x),
      onset_date.y = as_date(onset_date.y),
      onset_date.x = as_date(onset_date.x)
    ) %>%
    mutate(
      collection_date.x = ifelse(
        is.na(collection_date.x),
        collection_date.y,
        collection_date.x
      ),
      collection_date.y = ifelse(
        is.na(collection_date.y),
        collection_date.x,
        collection_date.y
      ),
      onset_date.x = ifelse(is.na(onset_date.x), onset_date.y, onset_date.x),
      onset_date.y = ifelse(is.na(onset_date.y), onset_date.x, onset_date.y)
    ) %>%
    mutate(
      collection_date.y = as_date(collection_date.y),
      collection_date.x = as_date(collection_date.x),
      onset_date.y = as_date(onset_date.y),
      onset_date.x = as_date(onset_date.x)
    ) %>%
    ## Looked and there were no collection date disagreements, so can get rid of the double copies
    dplyr::rename(collection_date = collection_date.x) %>%
    select(-collection_date.y) %>%
    ## Pull out those where either vax status or onset date are listed as different
    # mutate(
    #   vax_disagree = ifelse(vax.x != vax.y, TRUE, FALSE),
    #   onset_disagree = ifelse(onset_date.x != onset_date.y, TRUE, FALSE)
    # ) %>%
    select(-c(onset_date.x, vax.x)) %>% 
    dplyr::rename(onset_date = onset_date.y,
                  vax = vax.y) %>% 
    select(-c(has_specimen_id)) %>% 
    # Filtering out that one sample that is missing the metadata
    filter(!is.na(collection_date))
  
  return(flutes_unified_sequenced)
}