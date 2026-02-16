determineSimulatedPValues <- function(df4) {
  j_list <- list()
  for (j in 1:100) {
    i_list <- list()
    for (i in 1:100) {
      dist_1 <- df4 %>%
        filter(useful_order == j & type == "emp_a") %>%
        select(val) %>%
        as_vector()
      
      dist_2 <- df4 %>%
        filter(useful_order == i & type == "emp_b") %>%
        select(val) %>%
        as_vector()
      
      p_val <- wilcox.test(dist_1, dist_2)[[3]]
      
      sig <- tibble(dist_1_rep = i,
                    dist_2_rep = j,
                    p_val = p_val)
      
      i_list[[i]] <- sig
    }
    out <- bind_rows(i_list)
    j_list[[j]] <- out
  }
  
  df5 <- bind_rows(j_list) %>%
    mutate(sig = ifelse(p_val < 0.05, TRUE, FALSE))
  
  return(df5)
}