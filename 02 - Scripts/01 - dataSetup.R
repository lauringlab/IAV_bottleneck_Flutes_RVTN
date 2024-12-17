## Title: Creation of Input Files for Bottleneck Calculations
## Date Last Modified: 11/04/24
## Description: This code takes the  metadata files provided by CDC in and 
## merges them with the SNV data created by the lab. It then will pull consensus
## sequence data for all seasons and add it to the  data frame. Finally, it 
## creates files necessary for both doing the bottleneck calculation and to 
## create the plots.
## -----------------------------------------------------------------------------
library(tidyverse)

# 0. File paths ----------------------------------------------------------------
files.sources <- list.files(path = "./01 - Functions", pattern = "*.R", full.names = TRUE) %>%
  as_tibble() %>%
  filter(value != "./01 - Functions/README.txt") %>%
  as_vector()
sapply(files.sources, source)
rm(files.sources)
path_to_fulldat <- "~/Dropbox (University of Michigan)/CDC_RVTN_FLUTES_Flu/fulldat_lauring_flu2023-11-21.csv"
path_to_ddlabdat <- "~/Dropbox (University of Michigan)/CDC_RVTN_FLUTES_Flu/ddlabdat_lauring_flu2023-11-21.csv"
path_to_flutes_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Flutes/SNV_with_meta_data.csv"
path_to_rvtn_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/RVTN/SNV_with_mutation_type.csv"
path_to_flutes_meta <- "~/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/Vanderbilt_metadata_all_years.csv"

# 1. Data import ---------------------------------------------------------------
flutes_unified <- joinFlutesSnvToMeta(path_to_flutes_snv, path_to_flutes_meta, ctThresh = 100)
rvtn_unified <- joinRvtnSnvToMeta(path_to_fulldat, path_to_ddlabdat, path_to_rvtn_snv)
df <- bind_rows(rvtn_unified, flutes_unified)

rm(flutes_unified)
rm(rvtn_unified)

write.table(
  df,
  file = "./03 - Input/all_sequenced_samples.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

# 2. Clean data ----------------------------------------------------------------
df <- cleanUnifiedDataset(df, filter = TRUE, nSnvFilterThresh = 50)

# 3. Combine consensus information with iSNV dataset ---------------------------
all_cons <- createUnifiedConsensusData()

ids <- df %>%
  select(sample) %>%
  distinct() %>%
  as_vector()

all_cons <- all_cons %>%
  filter(sample %in% ids) %>%
  dplyr::rename(region = REGION, pos = POS, cons = CONS)

df <- df %>%
  left_join(all_cons)

# 5. Create transmission pairs -------------------------------------------------
final_pairs <- determineTransmissionPairs2(df)

# 6. Create input for plotting script ------------------------------------------
pair_meta_out <- final_pairs %>%
  select(
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
  file = "./03 - Input/pair_meta.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)
pairs <- 1:nrow(pair_meta_out) %>%
  data.frame()

write.table(
  pairs,
  file = "./03 - Input/pairs.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE
)

write.table(
  df,
  file = "./03 - Input/indsnv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

list_tv <- generateTvPlotInput(final_pairs, df)

tvPlotInput <- bind_rows(list_tv)
write.table(
  tvPlotInput,
  file = "./03 - Input/pairsnv_tv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE)

# 7. Create input for bottleneck calculation -----------------------------------
list2 <- generateBottleneckInput(final_pairs, df, save = TRUE)

all_snv <- bind_rows(list2)
write.table(
  all_snv,
  file = "./03 - Input/pairsnv.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)
