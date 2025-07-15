# Title: Bottleneck Calculation - Clonal Method
# Date Last Modified: 04/23/25
# Description: This code takes the cleaned input file and determines the mu,
# lambda, and bottleneck size using the clonal method as described in the Shi
# et al. paper
# -----------------------------------------------------------------------------
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
library(reshape2)
library(nbclonal)
library(parallel)
library(tidyverse)
source("~/git/FluTES_bottleneck/06 - Clonal Method Code/01 - Functions/determineMaxLLBottleneck.R")

# clonal_mut_all <- read_csv("/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_mut_all.csv")


# 1. Create the probability matrix used within the bottleneck calculation ------

listClonal <- list_clonal(
  n_values = 1:8,
  R0 = 11.1,
  mu_values = seq(0.01, 5.01, by = 0.01),
  maxMuGen = 50,
  maxFS = 50,
  clonal = 5
)

saveRDS(listClonal, file = "/Users/katykrupinsky/git/FluTES_bottleneck/06 - Clonal Method Code/03 - Input/listClonal_5mutations.rds")

listClonal <- readRDS("/Users/katykrupinsky/git/FluTES_bottleneck/06 - Clonal Method Code/03 - Input/listClonal_5mutations.rds")

# 2. Determine the actual bottleneck size --------------------------------------
bottlenecks <- list()

bottlenecks[[1]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Overall",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks[[2]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "17/18",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[3]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "18/19",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[4]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "19/20",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[5]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "21/22",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks[[6]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "H1N1",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[7]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "H3N2",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks[[8]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Adult-to-adult",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[9]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Adult-to-child",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[10]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Child-to-adult",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[11]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Child-to-child",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks[[12]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Both Female",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[13]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Both male",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[14]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Female-to-male",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[15]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Male-to-female",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks[[16]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Both",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[17]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Neither",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[18]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Donor only",
  createPlot = FALSE,
  r0Val = 11.1
)
bottlenecks[[19]] <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Recipient only",
  createPlot = FALSE,
  r0Val = 11.1
)

bottlenecks_by_factor_all <- bind_rows(bottlenecks)

saveOutput <- TRUE

if (saveOutput == TRUE) {
  write.csv(bottlenecks_by_factor_all, file = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_mut_all_threshhold_trimmed.csv')
}
