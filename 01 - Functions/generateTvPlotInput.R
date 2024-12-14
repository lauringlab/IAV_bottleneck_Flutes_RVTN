generateTvPlotInput <- function(final_pairs, df, save = FALSE) {
  list2 <- list()
  
  for (p in 1:nrow(final_pairs)) {
    # 1. Isolate pair ----------------------------------------------------------
    temp <- final_pairs %>%
      filter(pair_id == p)
    
    # 2. Isolate isnvs ---------------------------------------------------------
    donor_snv <- df %>%
      filter(sample == temp$donor_id) %>%
      dplyr::rename(
        donor_ref = ref,
        donor_alt = alt,
        donor_cons = cons,
        donor_freq = avg_freq
      ) %>%
      select(region, pos, donor_ref, donor_alt, donor_cons, donor_freq) %>%
      mutate(sample = temp$recipient_id) %>%
      left_join(all_cons) %>%
      dplyr::rename(recip_cons = cons) %>%
      relocate(recip_cons, .after = donor_cons) %>%
      select(-sample)
    
    recip_snv <- df %>%
      filter(sample == temp$recipient_id) %>%
      dplyr::rename(
        recip_ref = ref,
        recip_alt = alt,
        recip_cons = cons,
        recip_freq = avg_freq
      ) %>%
      select(region, pos, recip_ref, recip_alt, recip_cons, recip_freq) %>%
      mutate(sample = temp$donor_id) %>%
      left_join(all_cons) %>%
      dplyr::rename(donor_cons = cons) %>%
      relocate(donor_cons, .after = recip_cons) %>%
      select(-sample)
    
    # 3. Correct for different consensus ---------------------------------------
    pair_snv <- full_join(donor_snv, recip_snv) %>%
      mutate(pair_id = p) %>%
      relocate(pair_id) %>%
      select(-strain) %>%
      relocate(
        c(
          donor_ref,
          recip_ref,
          donor_alt,
          recip_alt,
          donor_cons,
          recip_cons,
          donor_freq,
          recip_freq
        ),
        .after = pos
      ) %>%
      mutate(
        only_donor = ifelse(is.na(recip_freq), TRUE, FALSE),
        only_recipient = ifelse(is.na(donor_freq), TRUE, FALSE),
        both = ifelse(!is.na(donor_freq) &
                        !is.na(recip_freq), TRUE, FALSE),
        same_ref = ifelse(donor_ref == recip_ref, TRUE, FALSE),
        same_alt = ifelse(donor_alt == recip_alt, TRUE, FALSE),
        same_cons = ifelse(donor_cons == recip_cons, TRUE, FALSE),
        dCons_rAlt = ifelse(donor_cons == recip_alt, TRUE, FALSE),
        dAlt_rCons = ifelse(donor_alt == recip_cons, TRUE, FALSE)
      ) %>%
      mutate(
        donor_freq = case_when(
          both == TRUE & same_cons == TRUE & same_alt == TRUE ~ donor_freq,
          both == TRUE & dCons_rAlt == TRUE & dAlt_rCons == TRUE ~ donor_freq,
          both == FALSE & only_donor == TRUE & same_cons == TRUE ~ donor_freq,
          both == FALSE & only_donor == TRUE & same_cons == FALSE & dAlt_rCons == TRUE ~ donor_freq,
          both == FALSE & only_donor == FALSE & same_cons == TRUE ~ 0,
          both == FALSE & only_donor == FALSE & same_cons == FALSE & dCons_rAlt == TRUE ~ 1,
          .default = NA
        ),
        recip_freq = case_when(
          both == TRUE & same_cons == TRUE & same_alt == TRUE ~ recip_freq,
          both == TRUE & dCons_rAlt == TRUE & dAlt_rCons == TRUE ~ 1 - recip_freq,
          both == FALSE & only_donor == TRUE & same_cons == TRUE ~ 0,
          both == FALSE & only_donor == TRUE & same_cons == FALSE & dAlt_rCons == TRUE ~ 1,
          both == FALSE & only_donor == FALSE & same_cons == TRUE ~ recip_freq,
          both == FALSE & only_donor == FALSE & same_cons == FALSE & dCons_rAlt == TRUE ~ recip_freq,
          .default = NA
        )
      ) %>%
      select(pair_id, region, pos, donor_freq, recip_freq)
    
    # 4. Save data in a list ---------------------------------------------------
    list2[[p]] <- pair_snv
    }
    return(list2)
  }