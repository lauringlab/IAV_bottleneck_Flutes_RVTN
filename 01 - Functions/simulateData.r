simulateData <- function(lambda_a_val = 1,
                         lambda_b_val = 3) {
  rztpois <- function(n, lambda) {
    x <- integer(0)
    while (length(x) < n) {
      y <- rpois(n - length(x), lambda)
      x <- c(x, y[y > 0])
    }
    x
  }
  
  set.seed(123)
  
  list <- list()
  list2 <- list()
  
  for (i in 1:100) {
    x1 <- rztpois(5, lambda = lambda_a_val)
    x3 <- rztpois(5, lambda = lambda_b_val)
    
    df <- tibble(emp_a = x1, emp_b = x3) %>%
      gather(type, val) %>%
      mutate(rep = i)
    
    df2 <- df %>%
      group_by(type) %>%
      summarise(
        median = median(val),
        q1 = quantile(val, 0.25),
        q3 = quantile(val, 0.75)
      ) %>%
      mutate(rep = i)
    
    list[[i]] <- df
    list2[[i]] <- df2
  }
  
  df <- bind_rows(list)
  df2 <- bind_rows(list2)
  
  return_obj <- list(raw_output = df, quantile_output = df2)
  
  return(return_obj)
}