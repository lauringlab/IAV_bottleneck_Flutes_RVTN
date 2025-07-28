## -----------------------------------------------------------------------------
## Title: Table 1 - Sample Characteristics
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
isnv_data <- read.table("/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/sample_data_with_meta.txt",
                 header = TRUE)

source("~/git/FluTES_bottleneck/01 - Functions/createIndMetaTable.R")
# 1. Create table --------------------------------------------------------------
isnv_ind_samples <- isnv_data %>% 
  dplyr::select(sample, hhsubid, hhid, site, season, age, sex, vax, strain) %>% 
  distinct()

table <- createIndMetaTable(isnv_ind_samples = isnv_ind_samples)
