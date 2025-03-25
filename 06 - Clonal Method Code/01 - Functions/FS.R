FS <- function(y, R0, mu) {
  k <- exp(-mu)
  meanR0 <- R0 * k
  if (y == 1) {
    prob <- 1 / (1 + meanR0 / k)^k
  } else if (y > 1) {
    prod <- 1
    for (j in 0:(y - 2)) {
      prod <- prod * (j / k + y)
    }
    prob <- prod/factorial(y) * (k/(meanR0 + k))^(k*y) * (meanR0*k/(meanR0+k))^(y-1)
  } else {
    prob <- 0
  }
  return(prob)
}
