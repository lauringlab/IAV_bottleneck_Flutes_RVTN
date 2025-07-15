formatClonalDataForPlotting <- function(pathToOutput = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_mut_all_threshhold_50edge.csv") {
  clonal_meta <- makePairTable() %>%
    dplyr::select(clonal, metric) %>%
    dplyr::filter(metric != 17 / 18) %>%
    mutate(metric = ifelse(metric == "pairs", "Overall", metric))
  
  clonal_method <- read_csv(pathToOutput) %>%
    filter(id == "max") %>%
    # # Remove 2017 samples because we have none
    # dplyr::filter(type != "17/18") %>%
    dplyr::select(-'...1') %>%
    dplyr::rename(metric = type) %>%
    mutate(
      metric = case_when(
        metric == "17/18" ~ "17",
        metric == "18/19" ~ "18",
        metric == "19/20" ~ "19",
        metric == "21/22" ~ "21",
        metric == "Adult-to-adult" ~ "Adult / Adult",
        metric == "Adult-to-child" ~ "Adult / Child",
        metric == "Child-to-adult" ~ "Child / Adult",
        metric == "Child-to-child" ~ "Child / Child",
        metric == "Both Female" ~ "Female / Female",
        metric == "Both male" ~ "Male / Male",
        metric == "Female-to-male" ~ "Female / Male",
        metric == "Male-to-female" ~ "Male / Female",
        metric == "Both" ~ "Vaccinated / Vaccinated",
        metric == "Neither" ~ "Unknown or unvaccinated / Unknown or unvaccinated",
        metric == "Donor only" ~ "Vaccinated / Unknown or unvaccinated",
        metric == "Recipient only" ~ "Unknown or unvaccinated / Vaccinated",
        .default = metric
      )
    )
  
  clonalData <- full_join(clonal_meta, clonal_method) %>%
    filter(metric != "households" & metric != "Samples used") %>%
    dplyr::rename(samples = clonal)
  
  clonalLL <- read_csv(pathToOutput)
  # %>%
  #   dplyr::filter(type != "17/18")
  
  factors <- unique(clonalLL$type)
  list <- list()
  for (i in 1:length(factors)) {
    f <- factors[i]
    list[[i]] <- findCI(clonalLL, factor = f)
  }
  
  clonalCi <- bind_rows(list) %>%
    dplyr::rename(
      max_LL = maxNb,
      lower_CI = lower,
      upper_CI = upper,
      level = factor,
      metric = factor
    ) %>%
    mutate(
      metric = case_when(
        metric == "Adult-to-adult" ~ "Adult / Adult",
        metric == "Adult-to-child" ~ "Adult / Child",
        metric == "Child-to-adult" ~ "Child / Adult",
        metric == "Child-to-child" ~ "Child / Child",
        metric == "Both Female" ~ "Female / Female",
        metric == "Both male" ~ "Male / Male",
        metric == "Female-to-male" ~ "Female / Male",
        metric == "Male-to-female" ~ "Male / Female",
        metric == "Both" ~ "Vaccinated / Vaccinated",
        metric == "Neither" ~ "Unknown or unvaccinated / Unknown or unvaccinated",
        metric == "Donor only" ~ "Vaccinated / Unknown or unvaccinated",
        metric == "Recipient only" ~ "Unknown or unvaccinated / Vaccinated",
        metric == "overall" ~ "Overall",
        .default = metric
      )
    ) %>%
    dplyr::mutate(metric = factor(
      metric,
      levels = c(
        "Overall",
        "17/18",
        "18/19",
        "19/20",
        "21/22",
        "H1N1",
        "H3N2",
        "Adult / Adult",
        "Adult / Child",
        "Child / Adult",
        "Child / Child",
        "Male / Male",
        "Male / Female",
        "Female / Male",
        "Female / Female",
        "Vaccinated / Unknown or unvaccinated",
        "Vaccinated / Vaccinated",
        "Unknown or unvaccinated / Unknown or unvaccinated",
        "Unknown or unvaccinated / Vaccinated"
      )
    ))
  
  clonalData2 <- clonalData %>%
    dplyr::mutate(
      metric = case_when(
        metric == "Adult-to-adult" ~ "Adult / Adult",
        metric == "Adult-to-child" ~ "Adult / Child",
        metric == "Child-to-adult" ~ "Child / Adult",
        metric == "Child-to-child" ~ "Child / Child",
        metric == "Both Female" ~ "Female / Female",
        metric == "Both male" ~ "Male / Male",
        metric == "Female-to-male" ~ "Female / Male",
        metric == "Male-to-female" ~ "Male / Female",
        metric == "Both" ~ "Vaccinated / Vaccinated",
        metric == "Neither" ~ "Unknown or unvaccinated / Unknown or unvaccinated",
        metric == "Donor only" ~ "Vaccinated / Unknown or unvaccinated",
        metric == "Recipient only" ~ "Unknown or unvaccinated / Vaccinated",
        metric == "overall" ~ "Overall",
        metric == "17" ~ "17/18",
        metric == "18" ~ "18/19",
        metric == "19" ~ "19/20",
        metric == "21" ~ "21/22",
        .default = metric
      )
    ) %>%
    dplyr::mutate(metric = factor(
      metric,
      levels = c(
        "Overall",
        "17/18",
        "18/19",
        "19/20",
        "21/22",
        "H1N1",
        "H3N2",
        "Adult / Adult",
        "Adult / Child",
        "Child / Adult",
        "Child / Child",
        "Male / Male",
        "Male / Female",
        "Female / Male",
        "Female / Female",
        "Vaccinated / Unknown or unvaccinated",
        "Vaccinated / Vaccinated",
        "Unknown or unvaccinated / Unknown or unvaccinated",
        "Unknown or unvaccinated / Vaccinated"
      )
    )) %>%
    dplyr::rename(sample_size = samples) %>%
    full_join(clonalCi) %>%
    dplyr::rename(level = metric)
  
  return(clonalData2)
}