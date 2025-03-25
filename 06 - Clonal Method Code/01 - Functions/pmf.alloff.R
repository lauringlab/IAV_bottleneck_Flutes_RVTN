# maxOff arguement defines the maximum number of offspring you want to calculate
pmf.alloff <- function(R0, mu, maxOff) {
  k_overall <- 1
  k_wt <- exp(-mu)
  k_mu <- (1 - exp(-mu))
  p <- 1 / (R0 + 1)
  pmf <- data.frame(
    x = 0:maxOff,
    overall = dnbinom(0:maxOff, k_overall, p),
    wildtype = dnbinom(0:maxOff, k_wt, p),
    mutant = dnbinom(0:maxOff, k_mu, p)
  )
  pmf <- melt(pmf, id.vars = "x")
  names(pmf) <- c("size", "type", "prob")
  return(pmf)
}
