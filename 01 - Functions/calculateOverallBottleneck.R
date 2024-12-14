calculateOverallBottleneck <- function(df) {
  max_num_snps <- max(df$n_variants)
  
  total_LL <- df %>%
    select(bottleneck_size, log_likelihood, n_variants) %>%
    mutate(adjusted_LL = (max_num_snps / n_variants) * log_likelihood) %>%
    group_by(bottleneck_size) %>%
    summarise(adjusted_LL = sum(adjusted_LL))
  
  Max_LL <- max(total_LL$adjusted_LL)
  Max_LL_bottleneck_index <-
    which(total_LL$adjusted_LL == max(total_LL$adjusted_LL))
  Max_LL_bottleneck <- total_LL$bottleneck_size[Max_LL_bottleneck_index]
  likelihood_ratio <- qchisq(0.95, df = 1)
  ci_tibble <- filter(total_LL, 2 * (Max_LL - adjusted_LL) <= likelihood_ratio)
  lower_CI_bottleneck <- min(ci_tibble$bottleneck_size)
  upper_CI_bottleneck <- max(ci_tibble$bottleneck_size)
  
  out <- tibble(max_LL = Max_LL_bottleneck,
                lower_CI = lower_CI_bottleneck,
                upper_CI = upper_CI_bottleneck)
  return(out)
  
}