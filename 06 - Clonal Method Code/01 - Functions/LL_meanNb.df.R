LL_meanNb.df <- function(df, listClonal, lambda_values, R0,
                         mu_values, maxMuGen, maxFS, maxIni) {
  prob <- 1:length(mu_values) %>%
    lapply(function(x) {
      1:length(lambda_values) %>%
        lapply(function(y) {
          LL_lambda(df, listClonal, lambda_values[y],
                    R0, mu_values[x], maxMuGen, maxFS, maxIni)
        })
    })
  names(prob) <- mu_values
  for (i in 1:length(mu_values)) {
    names(prob[[i]]) <- lambda_values
  }

  LLdf <- expand.grid(lambda = paste0("", lambda_values), mu = paste0("", mu_values))
  LLdf$prob <- unlist(prob)
  LLdf$lambda <- as.numeric(paste(LLdf$lambda))
  LLdf$mu <- as.numeric(paste(LLdf$mu))
  LLdf$meanN <- 0
  LLdf$meanNb <- 0

  for (j in lambda_values) {
    # calculate mean N
    dfPn <- pmf.ini_size(j, R0, maxIni)
    dfPn <- dfPn[dfPn$type == "adjusted",]
    dfPn$prob_N <- dfPn$N * dfPn$prob
    mean_n <- sum(dfPn$prob_N)
    LLdf[which(near(LLdf$lambda, j)),]$meanN <- mean_n

    # calculate mean Nb
    mean_nb <- 0
    for (k in 1:maxIni) {
      PNb <- 0
      for (n in k:maxIni) {
        PNb <- PNb + dfPn$prob[n+1] * dbinom(k, n, 1-1/R0) / (1-dbinom(0, n, 1-1/R0))
      }
      mean_nb <- mean_nb + k * PNb
    }
    LLdf[which(near(LLdf$lambda, j)),]$meanNb <- mean_nb
  }

  return(LLdf)
}
