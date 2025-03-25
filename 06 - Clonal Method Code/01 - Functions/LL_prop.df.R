LL_prop.df <- function(df,
                       listClonal,
                       prop_values,
                       R0,
                       mu_values,
                       maxMuGen,
                       maxFS,
                       maxIni) {
  prob <- 1:length(mu_values) %>%
    lapply(function(x) {
      1:length(prop_values) %>%
        lapply(function(y) {
          LL_prop(df,
                  listClonal,
                  prop_values[y],
                  R0,
                  mu_values[x],
                  maxMuGen,
                  maxFS)
        })
    })
  names(prob) <- mu_values
  for (i in 1:length(mu_values)) {
    names(prob[[i]]) <- prop_values
  }

  LLdf <- expand.grid(prop = paste0("", prop_values),
                      mu = paste0("", mu_values))
  LLdf$prob <- unlist(prob)
  LLdf$prop <- as.numeric(paste(LLdf$prop))
  LLdf$mu <- as.numeric(paste(LLdf$mu))
  return(LLdf)
}
