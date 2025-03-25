P_clonal <- function(n, m, R0, mu, maxMuGen, maxFS) {
  df_clonal_round <- pmf.P_clonal_round(n, R0, mu, maxMuGen, maxFS, m)[-1, ]
  px <- 1 / R0
  prob <- 0
  if (m == 0) {
    prob <- df_clonal_round[1, 2]
  } else {
    for (i in 1:m) {
      prob <- prob + df_clonal_round[i + 1, 2] *
        P_clonal(1, m - i, R0, mu, maxMuGen, maxFS) / (1 - px)
    }
  }
  return(prob)
}
