FSn <- function(n, y, R0, mu) {
  prob <- 0
  if (y >= n) {
    if (n == 1) {
      prob <- FS(y, R0, mu)
    } else if (n > 1) {
      for (i in 1:(y - n + 1)) {
        prob <- prob + FS(i, R0, mu) * FSn(n - 1, y - i, R0, mu)
      }
    }
  }
  return(prob)
}
