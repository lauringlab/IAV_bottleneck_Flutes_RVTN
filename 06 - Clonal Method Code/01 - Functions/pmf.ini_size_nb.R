pmf.ini_size_nb <- function(lambda, R0, maxIni, k) {
  df <- data.frame(N = 0:maxIni, nbin = dnbinom(0:maxIni, size = k, mu = lambda))
  df$adj_prob <- 0
  for (n in 0:maxIni) {
    Pest <- 1-1/R0^n
    df$adj_prob[n+1] <- df$nbin[n+1] * Pest
  }
  df$adj_prob <- df$adj_prob/sum(df$adj_prob)
  df <- melt(df, id.vars = "N")
  names(df) <- c("N", "type", "prob")
  df$type <- factor(df$type)
  levels(df$type) <- c("nbinom", "adjusted")
  return(df)
}
