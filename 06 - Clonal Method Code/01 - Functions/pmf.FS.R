# store pmf in a data frame
# maxFS arguement defines the maximum number of final size you want to calculate
pmf.FS <- function(R0, mu, maxFS) {
  prob <- rep(0, maxFS)
  for (i in 1:maxFS) {
    prob[i] <- FS(i, R0, mu)
  }
  names(prob) <- c(1:maxFS)
  df <- data.frame(FinalSize = names(prob), prob)
  df$FinalSize <- as.numeric(df$FinalSize)
  return(df)
}
