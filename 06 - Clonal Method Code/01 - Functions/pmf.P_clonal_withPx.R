pmf.P_clonal_withPx <- function(n, R0, mu, maxMuGen, maxFS, maxClonal) {
  df_clonal_round <- pmf.P_clonal_round(n,R0,mu,maxMuGen,maxFS,maxClonal)[-1, ]
  px <- 1 / R0
  prob <- rep(0, maxClonal + 2)
  prob[1] <- 1 / R0^n
  prob[2] <- df_clonal_round[1, 2]
  if (maxClonal != 0) {
    df_clonal_max_one_less <-
      pmf.P_clonal_withPx(1, R0, mu, maxMuGen, maxFS, maxClonal - 1)
    for (m in 1:maxClonal) {
      for (i in 1:m) {
        prob[m + 2] <- prob[m + 2] + df_clonal_round[i + 1, 2] *
          df_clonal_max_one_less[m - i + 2, 2]/(1 - px)
      }
    }
  }
  names(prob) <- c("x", 0:maxClonal)
  df <- data.frame(ClonalMu = names(prob), prob)
  return(df)
}
