LL_prop <- function(df, listClonal, prop, R0, mu, maxMuGen, maxFS) {
  prob <- 1
  list <- lapply(df$ClonalMu,
                   pmf.clonal_params,
                   listClonal = listClonal,
                   n_values = 1,
                   R0 = R0,
                   mu_values = mu,
                   maxMuGen = maxMuGen,
                   maxFS = maxFS)
  names(list) <- df$ClonalMu

  for (i in df$ClonalMu) {
    loc <- which(names(list) == i)
    dfClonalN <- list[[loc]]
    PclonalLam <- prop*dfClonalN$prob
    k <- df$freq[df$ClonalMu == i]
    prob <- prob * PclonalLam^k
  }
  prob <- log(prob)
  return(prob)
}
