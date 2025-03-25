# store pmf in a data frame
pmf.P_clonal_round <- function(n, R0, mu, maxMuGen, maxFS, maxClonal) {
  p1plus <- P1plus(n, R0, mu, maxMuGen, maxFS)
  prob <- rep(0, maxClonal + 2)
  prob[1] <- 1 / R0^n
  prob[2] <- P0(n, R0, mu, maxMuGen, maxFS)
  if (maxClonal != 0) {
    for (i in 1:maxClonal) {
      prob[i + 2] <- p1plus * dpois(i, mu) / (1 - dpois(0, mu))
    }
  }
  names(prob) <- c("x", 0:maxClonal)
  df <- data.frame(ClonalMu = names(prob), prob)
  return(df)
}
