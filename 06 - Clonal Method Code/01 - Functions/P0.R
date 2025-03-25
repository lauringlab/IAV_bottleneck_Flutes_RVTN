P0 <- function(n, R0, mu, maxMuGen, maxFS) {
  # S stands for the number of established mutant lineages
  S_inf <- P_wt_est(n, R0, mu)
  S_2plus <- (1 - P_mu_est(n, 0, R0, mu, maxMuGen, maxFS) -
                P_mu_est(n, 1, R0, mu, maxMuGen, maxFS)) * (1 - S_inf)
  prob <- S_inf + S_2plus
  return(prob)
}
