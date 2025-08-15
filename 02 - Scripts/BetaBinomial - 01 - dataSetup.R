## Title: Creation of Input Files for Bottleneck Calculations
## Date Last Modified: 07/28/25
## Description: This code takes the  metadata files provided by CDC in and
## merges them with the SNV data created by the lab. It then will pull consensus
## sequence data for all seasons and add it to the  data frame. Finally, it
## creates files necessary for both doing the bottleneck calculation and to
## create the plots.
## -----------------------------------------------------------------------------
library(tidyverse)
library(seqinr)

# 0. File paths ----------------------------------------------------------------
source("./01 - Functions/createUnifiedConsensusDataTrimmed.R")
source("./01 - Functions/joinFlutesSnvToMeta_new.R")
source("./01 - Functions/joinRvtnSnvToMeta_new.R")
source("./01 - Functions/getRvtnMetadata.R")
source("./01 - Functions/cleanUnifiedDataset.R")
source("./01 - Functions/determineTransmissionPairs2.R")
source("./01 - Functions/generateTvPlotInput.R")
source("./01 - Functions/generateBottleneckInput.R")

path_to_fulldat <- "./03 - Input/fulldat_min.csv"
path_to_ddlabdat <- "./03 - Input/ddlabdat_min.csv"
path_to_flutes_snv <- "./03 - Input/flutes_snv_min.csv"
path_to_rvtn_snv <- "./03 - Input/rvtn_snv_min.csv"
path_to_flutes_meta <- "./03 - Input/Vanderbilt_metadata_all_years.csv"

pathTo19SpecimenKeyForSequenced <- "./03 - Input/sequencing_ids_metadata_19_min.csv"
pathTo1718SpecimenKeyForSequenced <- './03 - Input/sequencing_ids_metadata_1718_min.csv'

pathToPassCoverageRvtn <- "./03 - Input/pass_coverage_rvtn.csv"
pathToPassCoverageFlutes <- "./03 - Input/pass_coverage_flutes.csv"

# 1. Determine which ones we have sequences for --------------------------------

all_cons <- createUnifiedConsensusDataTrimmed() %>%
  mutate(sample = as.numeric(sample))

sequenced_ids_df <- all_cons %>%
  ungroup() %>%
  dplyr::select(sample, strain) %>%
  distinct() %>%
  mutate(sequenced = TRUE) %>%
  na.omit()

# Filter for only ones that passed
flutes <- read_csv(pathToPassCoverageFlutes) %>%
  na.omit() %>%
  filter(sample != "Water") %>%
  mutate(sample = as.numeric(sample))
rvtn <- read_csv(pathToPassCoverageRvtn)

pass <- bind_rows(flutes, rvtn)

passed <- sequenced_ids_df %>%
  full_join(pass)

sequenced_ids_df <- passed %>%
  filter(Pass_coverage == "Pass" & sequenced == TRUE) %>%
  dplyr::select(sample, strain, sequenced)

# 1. Data import ---------------------------------------------------------------
flutes_unified <- joinFlutesSnvToMeta_new(
  path_to_flutes_snv,
  path_to_flutes_meta,
  path_to_ddlabdat,
  path_to_fulldat,
  pathTo1718SpecimenKeyForSequenced,
  pathTo19SpecimenKeyForSequenced,
  ctThresh = 100,
  sequenced_ids_df = sequenced_ids_df
)

rvtn_unified <- joinRvtnSnvToMeta_new(path_to_fulldat,
                                      path_to_ddlabdat,
                                      path_to_rvtn_snv,
                                      sequenced_ids_df = sequenced_ids_df)

df <- bind_rows(rvtn_unified, flutes_unified)

# 2. Clean data ----------------------------------------------------------------
df <- cleanUnifiedDataset(df, filter = TRUE, nSnvFilterThresh = 50)

# 3. Combine consensus information with iSNV dataset ---------------------------
ids <- df %>%
  dplyr::select(sample) %>%
  distinct() %>%
  as_vector()

all_cons2 <- all_cons %>%
  filter(sample %in% ids) %>% 
  ungroup() %>% 
  dplyr::select(sample, strain) %>% 
  distinct()

all_cons3 <- all_cons %>% 
  filter(sample %in% ids) %>% 
  ungroup() 

df <- df %>%
  left_join(all_cons2) %>% 
  left_join(all_cons3)

# 5. Create transmission pairs -------------------------------------------------
final_pairs <- determineTransmissionPairs2(df)

# 6. Create input for plotting script ------------------------------------------
write.table(
  df,
  file = "./04 - Output/iSNV_data/sample_data_with_meta.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

pair_meta_out <- final_pairs %>%
  dplyr::select(
    pair_id,
    donor_id,
    recipient_id,
    season,
    site,
    hhid,
    strain,
    donor_vax,
    recipient_vax,
    donor_age,
    recipient_age,
    donor_sex,
    recipient_sex,
    donor_onset,
    recipient_onset,
    donor_collection,
    recipient_collection
  ) %>%
  arrange(pair_id)

write.table(
  pair_meta_out,
  file = "./04 - Output/iSNV_data/pair_meta.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

pairs <- 1:nrow(pair_meta_out) %>%
  data.frame()

write.table(
  pairs,
  file = "./04 - Output/iSNV_data/pairs.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE
)

write.table(
  df,
  file = "./04 - Output/iSNV_data/indsnv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

list_tv <- generateTvPlotInput(final_pairs, df)
tvPlotInput <- bind_rows(list_tv)

write.table(
  tvPlotInput,
  file = "./04 - Output/iSNV_data/pairsnv_tv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

# 7. Create input for bottleneck calculation -----------------------------------
### NOTE: For input into the beta-binomial model, 0 values are not tolerated. To
### account for this, we take the inverse of each of the values. 

list2 <- generateBottleneckInput(final_pairs, df, save_loc = "./04 - Output/iSNV_data/00 - pairDataforCalculation/", save = TRUE)

all_snv <- bind_rows(list2)
write.table(
  all_snv,
  file = "./04 - Output/iSNV_data/pairsnv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

rm(list = setdiff(ls(), c()))
