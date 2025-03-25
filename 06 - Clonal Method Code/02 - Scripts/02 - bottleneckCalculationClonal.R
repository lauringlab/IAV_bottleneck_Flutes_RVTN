setwd('/Users/katykrupinsky/git/FluTES_bottleneck/06 - Clonal Method Code')
files <- list.files("./01 - Functions", full.names = TRUE)
lapply(files, source)
# ------------------------------------------------------------------------------
# Inputs for transmission bottleneck estimation
#
# df = dataset with first column named "ClonalMu" with number of clonal mut and
# sec column named "freq" with number of times each clonal mut num was observed
#
# listClonal = return from list_clonal function with same params as below
#
# lambda_values = sequence of means of Poisson distributions
#
# R0 = reproductive number
#
# mu_values = a sequence of mutation rates
#
# maxMuGen = maximum number of mutant lineages being calculated
#
# maxFS = maximum number of final sizes being calculated
#
# maxIni = maximum initial population size being calculated
#
# ------------------------------------------------------------------------------
# Inputs for list_clonal function that is an input for the bottleneck estimate
#
# n_values = sequence of initial population sizes
#
# R0 = reproductive number
#
# mu_values = sequence of mutation rates
#
# maxMuGen = maximum number of mutant lineages being calculated
#
# maxFS = Maximum number of final sizes being calculated
#
# clonal = Maximum number of clonal mutations being calculated
#
# ------------------------------------------------------------------------------
# 1. Create the probability matrix used within the bottleneck calculation ------

listClonal <- list_clonal(
  n_values = 1:8,
  R0 = 5.01,
  mu_values = seq(0.02, 5, by = 0.01),
  maxMuGen = 50,
  maxFS = 50,
  clonal = 15
)

# 2. Determine the actual bottleneck size --------------------------------------
bottlenecks <- list()

bottlenecks[[1]] <- determineMaxLLBottleneck(mutation_matrix, type = "overall")

bottlenecks[[2]] <- determineMaxLLBottleneck(mutation_matrix_17, type = "17")
bottlenecks[[3]] <- determineMaxLLBottleneck(mutation_matrix_18, type = "18")
bottlenecks[[4]] <- determineMaxLLBottleneck(mutation_matrix_19, type = "19")
bottlenecks[[5]] <- determineMaxLLBottleneck(mutation_matrix_21, type = "21")

bottlenecks[[6]] <- determineMaxLLBottleneck(mutation_matrix_H1N1, type = "H1N1")
bottlenecks[[7]] <- determineMaxLLBottleneck(mutation_matrix_H3N2, type = "H3N2")

bottlenecks[[8]] <- determineMaxLLBottleneck(mutation_matrix_adultToAdult, type = "adultToAdult")
bottlenecks[[9]] <- determineMaxLLBottleneck(mutation_matrix_adultToChild, type = "adultToChild")
bottlenecks[[10]] <- determineMaxLLBottleneck(mutation_matrix_childToAdult, type = "childToAdult")
bottlenecks[[11]] <- determineMaxLLBottleneck(mutation_matrix_childToChild, type = "childToChild")

bottlenecks[[12]] <- determineMaxLLBottleneck(mutation_matrix_bothFemale, type = "bothFemale")
bottlenecks[[13]] <- determineMaxLLBottleneck(mutation_matrix_bothMale, type = "bothMale")
bottlenecks[[14]] <- determineMaxLLBottleneck(mutation_matrix_femaleToMale, type = "femaleToMale")
bottlenecks[[15]] <- determineMaxLLBottleneck(mutation_matrix_maleToFemale, type = "maleToFemale")

bottlenecks[[16]] <- determineMaxLLBottleneck(mutation_matrix_both, type = "both")
bottlenecks[[17]] <- determineMaxLLBottleneck(mutation_matrix_neither, type = "neither")
bottlenecks[[18]] <- determineMaxLLBottleneck(mutation_matrix_donorOnly, type = "donorOnly")
bottlenecks[[19]] <- determineMaxLLBottleneck(mutation_matrix_recipientOnly, type = "recipientOnly")

bottlenecks_by_factor_all <- bind_rows(bottlenecks)

bottlenecks_by_factor_max <- bottlenecks_by_factor_all %>%
  filter(id == "max")

# write.csv(bottlenecks_by_factor_all, file = '/Users/katykrupinsky/git/FluTES_bottleneck/04 - Output/bottlenecks_by_factor_all.csv')

