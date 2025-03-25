P_mu_est <- function(n, m, R0, mu, maxMuGen, maxFS) {
  p_est <- 1 - 1 / R0
  prob <- rep(0, 1)
  pmf_mu_gen <- pmf.P_mu_n(n, R0, mu, maxMuGen, maxFS)
  for (i in m:maxMuGen) {
    p_m <- pmf_mu_gen[i + 1, 2]
    prob <- prob + dbinom(m, i, p_est) * p_m
  }
  return(prob)
}
