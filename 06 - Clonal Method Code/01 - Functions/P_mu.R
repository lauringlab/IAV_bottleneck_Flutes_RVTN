P_mu <- function(m, R0, mu) {
  prob <- 0
  for (i in 1:50) {
    k <- (1 - exp(-mu)) * i
    p <- 1 / (R0 + 1)
    NBmu <- dnbinom(m, k, p)
    prob <- prob + FSn_normal(1, i, R0, mu) * NBmu
  }
  return(prob)
}
