pmf.ini_size <- function(lambda, R0, maxIni) {
  df <- data.frame(N = 0:maxIni, pois = dpois(0:maxIni, lambda))
  df$adj_prob <- 0
  for (n in 0:maxIni) {
    Pest <- 1-1/R0^n
    df$adj_prob[n+1] <- df$pois[n+1] * Pest
  }
  df$adj_prob <- df$adj_prob/sum(df$adj_prob)
  df <- melt(df, id.vars = "N")
  names(df) <- c("N", "type", "prob")
  df$type <- factor(df$type)
  levels(df$type) <- c("poisson", "adjusted")
  return(df)
}
