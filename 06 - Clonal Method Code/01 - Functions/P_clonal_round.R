P_clonal_round <- function(n, m, R0, mu, maxMuGen, maxFS) {
  prob <- P1plus(n, R0, mu, maxMuGen, maxFS) * dpois(m, mu) / (1 - dpois(0, mu))
  return(prob)
}
