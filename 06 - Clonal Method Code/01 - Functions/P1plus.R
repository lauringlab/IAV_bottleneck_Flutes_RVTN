P1plus <- function(n, R0, mu, maxMuGen, maxFS) {
  p_wt_ext <- 1 - P_wt_est(n, R0, mu)
  prob <- P_mu_est(n, 1, R0, mu, maxMuGen, maxFS) * p_wt_ext
  return(prob)
}
