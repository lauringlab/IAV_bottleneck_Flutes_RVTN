P_transAndFix <- function(q, R0, maxIni) {
  freq <- q/100
  df <- data.frame(N = 1:maxIni, prob = 0)
  for (N in 1:maxIni) {
    P_trans_fix <- 0
    for (k in 1:N) {
      P_trans_fix <- P_trans_fix + dbinom(k, N, freq) * (1/R0)^(N-k) * (1-dbinom(0, k, 1-1/R0))
    }
    P_trans_fix <- P_trans_fix/(1-Px_analytical(N, R0))
    df$prob[N] <- P_trans_fix
  }
  return(df)
}
