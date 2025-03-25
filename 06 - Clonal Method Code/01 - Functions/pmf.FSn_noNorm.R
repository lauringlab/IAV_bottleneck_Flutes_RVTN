# store pmf in a data frame
pmf.FSn_noNorm <- function(n, R0, mu, maxFS) {
  df_n1 <- pmf.FS(R0, mu, maxFS)[1:maxFS, ]
  prob <- rep(0, maxFS)
  if (n == 1) {
    df <- df_n1
  } else if (n > 1) {
    df_n_one_less <- pmf.FSn_noNorm(n - 1, R0, mu, maxFS)
    for (i in (1:(maxFS - n))) {
      for (j in ((n - 1):(maxFS - i))) {
        prob[i + j] <- prob[i + j] + df_n1[i, 2] * df_n_one_less[j, 2]
      }
    }
    names(prob) <- seq(1, maxFS)
    df <- data.frame(FinalSize = names(prob), prob)
  }
  l <- nrow(df)
  sum_prob <- sum(df$prob)
  if (sum_prob < 1) {
    df[l + 1, 2] <- (1 - sum_prob)
  }
  return(df)
}
