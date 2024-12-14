joinFlutesSnvToMeta <- function(path_to_flutes_snv,
                                path_to_flutes_meta,
                                ctThresh = 30) {
  # 1. Format metadata ---------------------------------------------------------
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
  
  # 2. Format snv data ---------------------------------------------------------
  flutes_snv <- read.csv(path_to_flutes_snv)
  
  flutes_snv <- flutes_snv %>%
    filter(subtype_ct < ctThresh) %>%
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
    left_join(flutes_meta) %>%
    mutate(site = "nashville") %>%
    select(
      sample,
      hhsubid,
      hhid,
      site,
      season,
      age,
      sex,
      vax,
      collection_date,
      onset_date,
      region,
      pos,
      ref,
      alt,
      avg_freq
    )
  
  return(flutes_unified)
}