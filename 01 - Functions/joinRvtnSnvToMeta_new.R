joinRvtnSnvToMeta_new <- function(path_to_fulldat,
                              path_to_ddlabdat,
                              path_to_rvtn_snv,
                              sequenced_ids_df = sequenced_ids_df) {
  
  rvtn_sequenced_ids <- sequenced_ids_df %>%
    filter(!str_detect(as.character(sample), "^17|^18|^19"))
  
  # 1. Format metadata ---------------------------------------------------------
  meta <- getRvtnMetadata(path_to_fulldat, path_to_ddlabdat)
  
  rvtn_meta <- meta %>%
    filter(season == "2122") %>%
    dplyr::rename(
      sample = cdc_specid,
      age = age_at_enrollment,
      hhsubid = cdc_studyid,
      hhid = cdc_hhid,
      site = cdc_site,
      collection_date = date
    ) %>%
    mutate(
      sample = as.numeric(sample),
      season = as.numeric(str_sub(season, 1, 2)),
      hhsubid = as.numeric(hhsubid),
      hhid = as.numeric(hhid),
      vax = ifelse(vax == "Vaccinated", 1, ifelse(vax == "Unvaccinated", 0, 99))
    ) %>%
    dplyr::select(
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
      rvtn,
      flutes
    )
  
  # 2. Format snv data ---------------------------------------------------------
  rvtn_snv <- read.csv(path_to_rvtn_snv)
  
  rvtn_snv <- rvtn_snv %>%
    dplyr::select(sample, REGION, POS, REF, ALT, avg_freq, ALT_FREQ_1, ALT_FREQ_2, mutation_type) %>%
    dplyr::rename(
      ref = REF,
      region = REGION,
      pos = POS,
      alt = ALT,
      rep1_freq = ALT_FREQ_1,
      rep2_freq = ALT_FREQ_2
    )
  
  # 3. Join metadata and snv data ----------------------------------------------
  rvtn_unified <- rvtn_snv %>%
    full_join(rvtn_meta, multiple = "all") %>%
    dplyr::select(
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
      avg_freq,
      rep1_freq, 
      rep2_freq,
      mutation_type
    ) %>% 
    right_join(rvtn_sequenced_ids) 
  
  return(rvtn_unified)
}
