createIndMetaTable <- function(isnv_ind_samples = isnv_ind_samples) {
  
  list <- list()
  
  list[[1]] <- isnv_ind_samples %>%
    dplyr::select(hhid, season) %>%
    distinct() %>%
    group_by(season) %>%
    dplyr::count() %>%
    spread(season, n) %>%
    mutate(metric = "Households") %>%
    mutate(across(everything(), as.character))
  
  list[[2]] <- isnv_ind_samples %>%
    group_by(season) %>%
    dplyr::count() %>%
    spread(season, n) %>%
    mutate(metric = "Participants") %>%
    mutate(across(everything(), as.character))
  
  list[[3]] <- isnv_ind_samples %>%
    mutate(subtype = str_extract(strain, "(?<=_)[A-Z0-9]+(?=_)")) %>%
    group_by(season, subtype) %>%
    dplyr::count() %>%
    spread(season, n) %>%
    dplyr::rename(metric = subtype) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    mutate(across(everything(), as.character))
  
  list[[4]] <- isnv_ind_samples %>%
    group_by(season, vax) %>%
    mutate(vax = ifelse(is.na(vax) |
                          vax == 0, "Unknown or unvaccinated", "Vaccinated")) %>%
    dplyr::count() %>%
    ungroup() %>%
    group_by(season) %>%
    mutate(
      tot = sum(n),
      perc = round((n / tot) * 100, digits = 0),
      out = paste(n, " (", perc, ")", sep = "")
    ) %>%
    dplyr::select(season, vax, out) %>%
    spread(season, out) %>%
    mutate_all(~ replace(., is.na(.), "0 (0)")) %>%
    dplyr::rename(metric = vax)
  
  list[[5]] <- isnv_ind_samples %>%
    group_by(season, sex) %>%
    dplyr::count() %>%
    group_by(season) %>%
    mutate(
      tot = sum(n),
      perc = round((n / tot) * 100, digits = 0),
      out = paste(n, " (", perc, ")", sep = "")
    ) %>%
    dplyr::select(season, sex, out) %>%
    spread(season, out) %>%
    mutate_all(~ replace(., is.na(.), "0 (0)")) %>%
    dplyr::rename(metric = sex) %>%
    mutate(metric = ifelse(metric == 1, "Female", "Male"))
  
  list[[6]] <- isnv_ind_samples %>%
    mutate(age_dicot = ifelse(age < 18, "Child", "Adult")) %>%
    group_by(season, age_dicot) %>%
    dplyr::count() %>%
    group_by(season) %>%
    mutate(
      tot = sum(n),
      perc = round((n / tot) * 100, digits = 0),
      out = paste(n, " (", perc, ")", sep = "")
    ) %>%
    dplyr::select(season, age_dicot, out) %>%
    spread(season, out) %>%
    mutate_all(~ replace(., is.na(.), "0 (0)")) %>%
    dplyr::rename(metric = age_dicot)
  
  list[[7]] <- isnv_ind_samples %>% 
    group_by(season, hhid) %>% 
    dplyr::count() %>% 
    group_by(season, n) %>% 
    dplyr::count() %>% 
    dplyr::rename(metric = n,
                  freq = nn) %>% 
    mutate(metric = paste(metric, "individuals")) %>% 
    spread(season, freq) %>% 
    mutate_all(~ replace(., is.na(.), 0)) %>% 
    mutate(across(everything(), as.character))
    
    
    
  
  
  out <- bind_rows(list) %>% dplyr::select(metric, everything())
  
}