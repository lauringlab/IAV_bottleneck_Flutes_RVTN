createSimulatedPowerAnalysisInputData <- function(input_lambda_a_val, input_lambda_b_val) {
  source("./01 - Functions/simulateData.r")
  source("./01 - Functions/determineSimulatedPValues.r")
  source("./01 - Functions/plotDistributionsAndSignificance.r")
  
  dat <- simulateData(lambda_a_val = input_lambda_a_val, lambda_b_val = input_lambda_b_val)
  
  df3 <- dat$quantile_output %>%
    ungroup() %>%
    group_by(type) %>%
    arrange(median, q1, q3) %>%
    mutate(level_new_asc = row_number()) %>%
    arrange(-median, q1, q3) %>%
    mutate(level_new_desc = row_number())
  
  df4 <- dat$raw_output %>%
    full_join(df3) %>%
    mutate(useful_order = ifelse(type == "emp_a", level_new_asc, level_new_desc))
  
  df5 <- determineSimulatedPValues(df4 = df4)
  
  out <- list(df4, df5)
  
  return(out)
}