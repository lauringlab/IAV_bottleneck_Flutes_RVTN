P_mu_n <- function(n, m, R0, mu, maxFS) {
  prob <- 0
  df <- pmf.FSn(n, R0, mu, maxFS)
  for (i in 1:maxFS) {
    k <- (1 - exp(-mu)) * i
    p <- 1 / (R0 + 1)
    NBmu <- dnbinom(m, k, p)
    prob <- prob + df[i, 3] * NBmu
  }
  return(prob)
}
