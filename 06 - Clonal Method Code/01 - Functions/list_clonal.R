list_clonal <- function(n_values,
                        R0,
                        mu_values,
                        maxMuGen,
                        maxFS,
                        clonal) {
  listClonal <- 1:length(mu_values) %>%
    lapply(function(x) {
      1:length(n_values) %>%
        lapply(function(y) {
          pmf.P_clonal(n_values[y], R0, mu_values[x], maxMuGen, maxFS, clonal)
        })
    })
  # names the elements in the list with mu and n values
  names(listClonal) <- mu_values
  for (i in 1:length(mu_values)) {
    names(listClonal[[i]]) <- n_values
  }
  return(listClonal)
}
