pmf.FSn <- function(n, R0, mu, maxFS) {
  df_n1 <- pmf.FS(R0, mu, maxFS)
  prob <- rep(0, maxFS)
  if (n == 1) {
    df <- df_n1
  } else if (n > 1) {
    df_n_one_less <- pmf.FSn(n - 1, R0, mu, maxFS)
    for (i in (1:(maxFS - n))) {
      for (j in ((n - 1):(maxFS - i))) {
        prob[i + j] <- prob[i + j] + df_n1[i, 2] * df_n_one_less[j, 2]
      }
    }
    names(prob) <- seq(1, maxFS)
    df <- data.frame(FinalSize = names(prob), prob)
  }
  pi <- max((1 - P_wt_est(n, R0, mu)), sum(df$prob))
  df$prob_normal <- df$prob / pi
  df$FinalSize <- as.numeric(df$FinalSize)
  return(df)
}
