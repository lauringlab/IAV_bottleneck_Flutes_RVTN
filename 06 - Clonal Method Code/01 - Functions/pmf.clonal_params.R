pmf.clonal_params <- function(clonal, listClonal, n_values, R0, mu_values, maxMuGen, maxFS) {
  df <- expand.grid(n = paste0("", n_values), mu = paste0("", mu_values))
  df$prob <- 0

  for (N in n_values) {
    for (MU in mu_values) {
      muloc <- which(names(listClonal) == MU)
      nloc <- which(names(listClonal[[muloc]]) == N)
      prob <- listClonal[[muloc]][[nloc]]$prob[clonal+1]
      df$prob[which(df$n == N & df$mu == MU)] <- prob
    }
  }

  df$n <- as.numeric(df$n)
  df$mu <- as.numeric(paste(df$mu))
  return(df)
}
