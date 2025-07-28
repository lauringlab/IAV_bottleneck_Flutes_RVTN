## -----------------------------------------------------------------------------
## Title: Table 2 - Pair Characteristics
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
source("~/git/FluTES_bottleneck/01 - Functions/makePairTable.R")
isnv_pair <- read.table(
  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt",
  header = TRUE
) %>%
  # Remove the pairs that were removed from the dataset due to no iSNVs
  filter(pair_id != 55 & pair_id != 62 & pair_id != 63)

clonal_pair <- read.table(
  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_dist_with_meta.txt",
  header = TRUE
)

# 1. Create table --------------------------------------------------------------
table2 <- makePairTable(isnv_pair = isnv_pair,
                        clonal_pair = clonal_pair)
