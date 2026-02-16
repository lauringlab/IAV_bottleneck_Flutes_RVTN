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

source("./01 - Functions/createFigurePanel.r")
source("./01 - Functions/determineMaxLLBottleneck.r")

# 1. Format data ---------------------------------------------------------------
listClonal <- list_clonal(
  n_values = 1:8,
  R0 = 11.1,
  mu_values = seq(0.01, 4, by = 0.01),
  maxMuGen = 50,
  maxFS = 50,
  clonal = 18
)

clonal_mut_all <- read_csv("./04 - Output/Clonal_data/clonal_mut_all.csv")

clonalVals <- determineMaxLLBottleneck(
  listClonal = listClonal,
  mutation_matrix = clonal_mut_all,
  type = "Overall",
  createPlot = FALSE,
  r0Val = 11.1
)

clonalValsImp <- clonalVals %>% 
  filter(mu == 1.33) %>% 
  select(lambda, meanNb)

dat <- simulateData(lambda_a_val = input_lambda_a_val, lambda_b_val = input_lambda_b_val)

df3 <- dat$quantile_output %>%
  ungroup() %>%
  group_by(type) %>%
  arrange(median, q1, q3) %>%
  mutate(level_new_asc = row_number()) %>%
  arrange(-median, q1, q3) %>%
  mutate(level_new_desc = row_number())

df4 <- dat$raw_output %>%
  full_join(df3) %>%
  mutate(useful_order = ifelse(type == "emp_a", level_new_asc, level_new_desc))

df5 <- determineSimulatedPValues(df4 = df4)



a <- createFigurePanel(
  input_lambda_a_val = 1,
  input_lambda_b_val = 3,
  input_b_col = "#984136"
)
b <- createFigurePanel(
  input_lambda_a_val = 1,
  input_lambda_b_val = 5,
  input_b_col = "#c26a7a"
)
c <- createFigurePanel(
  input_lambda_a_val = 1,
  input_lambda_b_val = 7,
  input_b_col = "#ecc0a1"
)

p <- a / b / c + plot_annotation(tag_levels = "A")

p

save <- TRUE

if (save) {
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/suppfig5.png",
    width = 20,
    height = 15
  )
}
