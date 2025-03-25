# store pmf in a data frame
pmf.P_mu_n <- function(n, R0, mu, maxMuGen, maxFS) {
  prob <- rep(0, maxMuGen + 1)
  df_FS <- pmf.FSn(n, R0, mu, maxFS)
  for (i in 0:maxMuGen) {
    prob_mugen <- 0
    for (j in 1:maxFS) {
      k <- (1 - exp(-mu)) * j
      p <- 1 / (R0 + 1)
      NBmu <- dnbinom(i, k, p)
      prob_mugen <- prob_mugen + df_FS[j, 3] * NBmu
    }
    prob[i + 1] <- prob[i + 1] + prob_mugen
  }
  names(prob) <- c(0:maxMuGen)
  df <- data.frame(MuLinGen = names(prob), prob)
  df$MuLinGen <- as.numeric(df$MuLinGen)
  return(df)
}
