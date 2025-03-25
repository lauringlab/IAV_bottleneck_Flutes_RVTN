FSn_normal <- function(n, y, R0, mu) {
  prob <- 0
  if (n == 1) {
    prob <- FS(y, R0, mu)
  } else if (n > 1) {
    for (i in 1:y - n + 1) {
      prob <- prob + FS(i, R0, mu) * FSn(n - 1, y - i, R0, mu)
    }
  } else {
    prob <- 0
  }
  pi <- 1 - P_wt_est(n, R0, mu)
  prob_normal <- prob / pi
  return(prob_normal)
}
