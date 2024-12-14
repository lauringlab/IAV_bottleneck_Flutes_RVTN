calculateBottleneckByMetadata <- function(df2) {
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
    mutate(
      meta_factor_lev = case_when(
        meta_factor == "season" ~ 1,
        meta_factor == "subtype" ~ 2,
        meta_factor == "age_cat" ~ 3,
        meta_factor == "sex_cat" ~ 4,
        meta_factor == "vax_cat" ~ 5
      )
    ) %>%
    arrange(meta_factor_lev, level) %>%
    mutate(x = row_number(),
           no_conf = ifelse(lower_CI == upper_CI, 1, 0))
}