# store pmf in a data frame
pmf.P_mu <- function(R0, mu) {
  prob <- rep(0, 51)
  for (i in seq(0, 50, by = 1)) {
    prob[i + 1] <- prob[i + 1] + P_mu(i, R0, mu)
  }
  names(prob) <- seq(0, 50)
  df <- data.frame(MuOffspring = names(prob), Probability = prob)
  return(df)
}
