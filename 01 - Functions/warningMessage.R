warningMessage <- function(opt = 0,
                           suppressMessage = TRUE,
                           new_row_count = numeric(),
                           original_row_count = numeric(),
                           Max_LL_bottleneck = numeric(),
                           bottleneck_values_vector = vector(),
                           warnings = tibble()) {
  if (opt == 0) {
    print("Invalid Input")
  } else if (opt == 1) {
    if (new_row_count != original_row_count) {
      warning <- "WARNING:  Rows of input file with donor frequency less than variant calling threshold have been removed during analysis."
    } else
      (return(warnings))
  } else if (opt == 2) {
    if (max(Max_LL_bottleneck) == max(bottleneck_values_vector)) {
      warning <- "Peak bottleneck value for MLE is at Nb_max (or largest possible value given Nb_increment)!  Try raising Nb_max for better bottleneck estimate"
    } else if (min(Max_LL_bottleneck) == min(bottleneck_values_vector)) {
      if (min(bottleneck_values_vector) > 1) {
        warning <- "Peak bottleneck value for MLE is at Nb_min (or smallest possible value given Nb_increment)!  Try lowering Nb_min for better bottleneck estimate"
      } else {
        return(warnings)
      }
    } else {
      return(warnings)
    }
  }
  
  if (suppressMessage == FALSE) {
    print(warning)
  }
  warnings <- tibble(pair_id = p, warnings = warning) %>%
    bind_rows(warnings)
  return(warnings)
}