findCI <- function(raw, factor) {
  df <- raw %>%
    dplyr::filter(type == factor) %>%
    dplyr::select(meanNb, lambda, mu, prob) %>%
    dplyr::rename(theta = meanNb, ll = prob)
  
  maxLL <- max(df$ll)
  maxMuLL <- as_vector(df[df$ll == maxLL, "mu"])
  
  LL_tibble <- df %>%
    dplyr::filter(mu == maxMuLL) %>%
    arrange(mu)
  
  Max_LL <- max(LL_tibble$ll)
  Max_LL_bottleneck_index <- which(LL_tibble$ll == max(LL_tibble$ll))
  Max_LL_bottleneck <- LL_tibble$theta[Max_LL_bottleneck_index]
  likelihood_ratio <- qchisq(0.95, df = 1)
  
  ci_tibble <- LL_tibble %>%
    dplyr::filter(2 * (Max_LL - ll) < likelihood_ratio)
  lower_CI_bottleneck <- min(ci_tibble$theta)
  upper_CI_bottleneck <- max(ci_tibble$theta)
  
  
  out <- data.frame(
    upper = upper_CI_bottleneck,
    lower = lower_CI_bottleneck,
    maxNb = Max_LL_bottleneck,
    factor = factor
  )
  
  return(out)
}