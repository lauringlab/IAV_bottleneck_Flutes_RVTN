## -----------------------------------------------------------------------------
## Title: Table 2 - Pair Characteristics
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------
library(tidyverse)

# 0. Load libraries and import data --------------------------------------------
source("./01 - Functions/makePairTable.R")

nonzero_pairs <- read.table("./04 - Output/iSNV_data/01 - Output/num_vars.txt", header = TRUE) %>%
  dplyr::filter(n_variants != 0) %>%
  dplyr::select(pair) %>%
  as_vector()

isnv_pair <- read.table("./04 - Output/iSNV_data/pair_meta.txt", header = TRUE) %>%
  # Remove the pairs that were removed from the dataset due to no iSNVs
  filter(pair_id %in% nonzero_pairs)

clonal_pair <- read.table("./04 - Output/Clonal_data/clonal_dist_with_meta.txt",
                          header = TRUE)

# 1. Create table --------------------------------------------------------------
table2 <- makePairTable(isnv_pair = isnv_pair, clonal_pair = clonal_pair)
