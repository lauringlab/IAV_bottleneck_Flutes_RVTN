pmf.P_clonal_3 <- function(n, R0, mu, maxMuGen, maxFS) {
  prob <- rep(0, 3)
  prob[1] <- Px_analytical(n, R0)
  prob[2] <- P0(n, R0, mu, maxMuGen, maxFS)
  prob[3] <- P1plus(n, R0, mu, maxMuGen, maxFS)
  names(prob) <- c("Px", "P0", "P1plus")
  df <- data.frame(Type = names(prob), Probability = prob)
  return(df)
}
