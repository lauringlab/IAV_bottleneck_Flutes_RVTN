calculateBottleneckByMetadata <- function(pathToConfidenceInterval = "04 - Output/confidence_int.txt",
                                          pathToLogLikelihood = "04 - Output/logLikelihood.txt",
                                          pathToNumVars = "04 - Output/num_vars.txt",
                                          pathToPairMeta = "03 - Input/pair_meta.txt",
                                          noFive = FALSE) {
  ### Get our data frames in order ###
  confidence_int <- read.table(pathToConfidenceInterval, header = TRUE)
  logLikelihood <- read.table(pathToLogLikelihood, header = TRUE)
  num_vars <- read.table(pathToNumVars, header = TRUE)
  pair_meta <- read.table(pathToPairMeta, header = TRUE)
  
  bottleneck <- confidence_int %>% full_join(num_vars) %>% dplyr::rename(pair_id = pair)
  bottleneck_meta <- bottleneck %>% full_join(pair_meta)
  
  bottleneck_meta2 <- bottleneck_meta %>%
    mutate(
      subtype = str_extract(strain, "H\\d+N\\d+"),
      season = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22"
      )
    ) %>%
    filter(max_LL != 999) %>%
    mutate(age_diff = donor_age - recipient_age) %>%
    mutate(
      age_cat = case_when(
        donor_age < 18 & recipient_age < 18 ~ "Child-to-child",
        donor_age < 18 & recipient_age >= 18 ~ "Child-to-adult",
        donor_age >= 18 & recipient_age >= 18 ~ "Adult-to-adult",
        donor_age >= 18 & recipient_age < 18 ~ "Adult-to-child"
      ),
      vax_cat = case_when(
        donor_vax == 0 & recipient_vax == 0 ~ "Neither",
        donor_vax == 0 &
          recipient_vax == 1 ~ "Recipient only",
        donor_vax == 1 & recipient_vax == 0 ~ "Donor only",
        donor_vax == 1 & recipient_vax == 1 ~ "Both"
      ),
      sex_cat = case_when(
        donor_sex == 1 & recipient_sex == 1 ~ "Both Female",
        donor_sex == 1 & recipient_sex == 2 ~ "Female-to-male",
        donor_sex == 2 & recipient_sex == 1 ~ "Male-to-female",
        donor_sex == 2 & recipient_sex == 2 ~ "Both male"
      )
    )
  
  dup <- tibble() %>%
    mutate(dup_id = row_number())
  
  ### Bring in log likelihood values ###
  df <- logLikelihood %>%
    full_join(confidence_int) %>%
    full_join(num_vars) %>%
    dplyr::rename(log_likelihood = Log_Likelihood)
  
  df2 <- df %>%
    dplyr::rename(pair_id = pair) %>%
    full_join(bottleneck_meta2)
  
  if(noFive == TRUE) {
    df <- df %>% filter(pair != 20)
    df2 <- df2 %>% filter(donor_id != 1809801502 & recipient_id != 1809802501)
  }
  
  # Actual weighted average bottleneck calculation -----------------------------
  
  ### Calculate overall ###
  overall_bottleneck <- calculateOverallBottleneck(df) %>% 
    mutate(level = "overall", meta_factor = "overall")
  
  ### Calculate by season ###
  bottleneck_by_season <- calculateBottleneckByLevels(
    df2,
    var_int = "season",
    levels_int = c("17/18", "18/19", "19/20", "21/22")
  )
  
  ### Calculate by subtype ###
  bottleneck_by_subtype <- calculateBottleneckByLevels(df2,
                                                       var_int = "subtype",
                                                       levels_int = c("H1N1", "H3N2"))
  
  ### Calculate by Age Pairing ###
  bottleneck_by_age <- calculateBottleneckByLevels(
    df2,
    var_int = "age_cat",
    levels_int = c(
      "Adult-to-child",
      "Child-to-child",
      "Child-to-adult",
      "Adult-to-adult"
    )
  )
  
  ### Calculate by Vax Pairing ###
  bottleneck_by_vax <- calculateBottleneckByLevels(
    df2,
    var_int = "vax_cat",
    levels_int = c("Neither", "Both", "Recipient only", "Donor only")
  )
  
  ### Calculate by Age Pairing ###
  bottleneck_by_sex <- calculateBottleneckByLevels(
    df2,
    var_int = "sex_cat",
    levels_int = c(
      "Both male",
      "Both Female",
      "Male-to-female",
      "Female-to-male"
    )
  )
  
  bottleneck_by_metadata <- bottleneck_by_season %>%
    bind_rows(bottleneck_by_subtype) %>%
    bind_rows(bottleneck_by_age) %>%
    bind_rows(bottleneck_by_sex) %>%
    bind_rows(bottleneck_by_vax) %>%
    bind_rows(overall_bottleneck) %>% 
    mutate(
      meta_factor_lev = case_when(
        meta_factor == "season" ~ 1,
        meta_factor == "subtype" ~ 2,
        meta_factor == "age_cat" ~ 3,
        meta_factor == "sex_cat" ~ 4,
        meta_factor == "vax_cat" ~ 5,
        meta_factor == "overall" ~ 0
      )
    ) %>%
    arrange(meta_factor_lev, level) %>%
    mutate(x = row_number(),
           no_conf = ifelse(lower_CI == upper_CI, 1, 0))
}