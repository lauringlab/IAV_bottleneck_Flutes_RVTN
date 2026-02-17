## -----------------------------------------------------------------------------
## Title: Supplemental Figure 5 - Power Analysis
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and functions ----------------------------------------------
library(tidyverse)
library(ggridges)
library(ggExtra)
library(cowplot)
library(nbclonal)
library(parallel)
library(patchwork)

source("./01 - Functions/createSimulatedPowerAnalysisInputData.r")
source("./01 - Functions/plotDistributionsAndSignificance.r")
source("./01 - Functions/determineMaxLLBottleneck.r")

# 1. Simulate data -------------------------------------------------------------
simulated_a <- createSimulatedPowerAnalysisInputData(input_lambda_a_val = 1, input_lambda_b_val = 3)
simulated_b <- createSimulatedPowerAnalysisInputData(input_lambda_a_val = 1, input_lambda_b_val = 5)
simulated_c <- createSimulatedPowerAnalysisInputData(input_lambda_a_val = 1, input_lambda_b_val = 7)

a <- plotDistributionsAndSignificance(
  df4 = simulated_a[[1]],
  df5 = simulated_a[[2]],
  lambda_a_val = 1,
  lambda_b_val = 3,
  b_col = "#984136"
)

b <- plotDistributionsAndSignificance(
  df4 = simulated_b[[1]],
  df5 = simulated_b[[2]],
  lambda_a_val = 1,
  lambda_b_val = 5,
  b_col = "#c26a7a"
)
c <- plotDistributionsAndSignificance(
  df4 = simulated_c[[1]],
  df5 = simulated_c[[2]],
  lambda_a_val = 1,
  lambda_b_val = 7,
  b_col = "#ecc0a1"
)

p <- a / b / c + plot_annotation(tag_levels = "A")

save <- TRUE

if (save) {
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/tif/suppfig5.tif",
    width = 20,
    height = 15
  )
  
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/eps/suppfig5.eps",
    width = 20,
    height = 15
  )
  
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/png/suppfig5.png",
    width = 20,
    height = 15
  )
}
