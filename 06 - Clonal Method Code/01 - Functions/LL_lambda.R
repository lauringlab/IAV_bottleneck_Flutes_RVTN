LL_lambda <- function(df, listClonal, lambda, R0, mu, maxMuGen, maxFS, maxIni) {
  df$prob <- df$freq / sum(df$freq)
  dfPn <- pmf.ini_size(lambda, R0, maxIni)
  dfPn <- dfPn[dfPn$type == "adjusted",]
  prob <- 1

  list <- lapply(df$ClonalMu,
                   pmf.clonal_params,
                   listClonal = listClonal,
                   n_values = 1:maxIni,
                   R0 = R0,
                   mu_values = mu,
                   maxMuGen = maxMuGen,
                   maxFS = maxFS)
  names(list) <- df$ClonalMu

  for (i in df$ClonalMu) {
    loc <- which(names(list) == i)
    dfClonalN <- list[[loc]]
    PclonalLam <- 0
    for (n in 1:maxIni) {
      Pn <- dfPn$prob[dfPn$N == n]
      PclonalN <- dfClonalN$prob[dfClonalN$n == n]
      PclonalLam <- PclonalLam + Pn*PclonalN
    }
    k <- df$freq[df$ClonalMu == i]
    prob <- prob * PclonalLam^k
  }
  prob <- log(prob)
  return(prob)
}
