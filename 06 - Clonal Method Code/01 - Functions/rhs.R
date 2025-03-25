rhs <- function(p, R0, mu) {
  rhs <- 1 / (1 + (R0 * exp(-mu) * (1 - p)) / (exp(-mu)))^exp(-mu)
  return(rhs)
}
