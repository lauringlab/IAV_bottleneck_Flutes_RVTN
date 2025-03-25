P_wt_est <- function(n, R0, mu) {
  lhs <- function(p) {p}
  rhs <- function(p, R0, mu) {
    rhs <- 1 / (1 + (R0 * exp(-mu) * (1 - p)) / (exp(-mu)))^exp(-mu)
    return(rhs)
  }
  uni <- try(uniroot(function(p) rhs(p, R0, mu) - lhs(p), c(0, 0.9),
                     extendInt = "yes"), silent = TRUE)
  uni <- if (inherits(uni, "try-error")) 1 else uni$root
  if (uni < 1 & uni > 0) {
    ext_1 <- uni
  } else {
    ext_1 <- 1
  }
  est_n <- 1 - ext_1^n
  return(est_n)
}
