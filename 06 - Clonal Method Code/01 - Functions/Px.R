Px <- function(n, R0, mu, maxMuGen, maxFS) {
  p_wt_ext <- 1 - P_wt_est(n, R0, mu)
  p_mu_ext <- P_mu_est(n, 0, R0, mu, maxMuGen, maxFS)
  prob <- p_mu_ext * p_wt_ext
  return(prob)
}
