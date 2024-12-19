cleanUnifiedDataset <- function(df,
                                filter = TRUE,
                                nSnvFilterThresh = 50) {
  # 1. Imputing onset date as the collection date if onset date is missing -----
  out <- df %>%
    mutate(onset_date = if_else(is.na(onset_date), collection_date, onset_date))
  
  # 2. Filter for indeterminate vaccination status -----------------------------
  # out <- out %>% 
  #   filter(vax == 0 | vax == 1)
  
  # 3. Apply filter for too many iSNVs -----------------------------------------
  if (filter == TRUE) {
    num_snv <- out %>%
      group_by(sample) %>%
      count() %>%
      filter(n < nSnvFilterThresh) %>%
      select(sample) %>% 
      as_vector()
    
    out <- out %>% 
      filter(sample %in% num_snv)
  
  # 4. Filter for only hhids with more than one person -------------------------
  house <- out %>%
    select(hhsubid, hhid) %>%
    distinct() %>%
    group_by(hhid) %>%
    count() %>%
    filter(n > 1) %>%
    select(hhid) %>%
    as_vector()
  
  out <- out %>% 
    filter(hhid %in% house)
  
  # 5. Filter for first samples from each individual ---------------------------
  first <- out %>% 
    mutate(collection = ifelse(season == 21, 5, str_sub(sample, 8, 8))) %>%
    mutate(collection = ifelse(collection == 5, 1, 2)) %>% 
    select(hhid, hhsubid, collection_date, sample, collection) %>% 
    distinct() %>% 
    arrange(hhsubid, collection_date, collection) %>% 
    group_by(hhsubid) %>% 
    mutate(sample_num = row_number()) %>% 
    ungroup() %>% 
    filter(sample_num == 1) %>% 
    select(sample) %>% 
    as_vector()
  
  out <- out %>% 
    filter(sample %in% first)
    
  }
  
  return(out)
}