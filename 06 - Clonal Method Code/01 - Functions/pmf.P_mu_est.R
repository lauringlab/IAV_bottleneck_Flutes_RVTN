# store pmf in a data frame
pmf.P_mu_est <- function(n, R0, mu, maxMuGen, maxFS) {
  p_est <- 1 - 1 / R0
  df_mu_gen <- pmf.P_mu_n(n, R0, mu, maxMuGen, maxFS)
  prob <- rep(0, maxMuGen + 1)
  for (j in 0:maxMuGen) {
    p_m <- df_mu_gen[j + 1, 2]
    for (i in 0:j) {
      prob[i + 1] <- prob[i + 1] + dbinom(i, j, p_est) * p_m
    }
  }
  names(prob) <- c(0:maxMuGen)
  df <- data.frame(MuLinEst = names(prob), prob)
  df$MuLinEst <- as.numeric(df$MuLinEst)
  return(df)
}
