calculateBottleneckByLevels <- function(df2,
                                        var_int = "season",
                                        levels_int = c("17/18", "18/19", "19/20", "21/22")) {
  list <- list()
  
  for (s in 1:length(levels_int)) {
    sub <- df2 %>%
      filter(.data[[var_int]] == levels_int[s])
    
    ss <- n_distinct(sub$pair_id)
    
    list[[s]] <- calculateOverallBottleneck(sub) %>%
      mutate(level = levels_int[s],
             meta_factor = var_int,
             sample_size = ss)
  }
  out <- bind_rows(list)
  return(out)
}