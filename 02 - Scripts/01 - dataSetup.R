## Title: Creation of Input Files for Bottleneck Calculations
## Date Last Modified: 11/04/24
## Description: This code takes the  metadata files provided by CDC in and
## merges them with the SNV data created by the lab. It then will pull consensus
## sequence data for all seasons and add it to the  data frame. Finally, it
## creates files necessary for both doing the bottleneck calculation and to
## create the plots.
## -----------------------------------------------------------------------------
library(tidyverse)
library(seqinr)
setwd("/Users/katykrupinsky/git/FluTES_bottleneck")

# 0. File paths ----------------------------------------------------------------
files.sources <- list.files(path = "./01 - Functions",
                            pattern = "*.R",
                            full.names = TRUE) %>%
  as_tibble() %>%
  filter(value != "./01 - Functions/README.txt") %>%
  as_vector()
sapply(files.sources, source)
rm(files.sources)
path_to_fulldat <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/CDC_RVTN_FLUTES_Flu/CDC_RVTN_FLUTES_Flu_2/fulldat_lauring_flu2023-12-07.csv"
path_to_ddlabdat <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/CDC_RVTN_FLUTES_Flu/CDC_RVTN_FLUTES_Flu_2/ddlabdat_lauring_flu2023-12-07.csv"
path_to_flutes_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Flutes/SNV_with_meta_data.csv"
path_to_rvtn_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/RVTN/SNV_with_mutation_type.csv"
path_to_flutes_meta <- "~/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/Vanderbilt_metadata_all_years.csv"
pathTo19SpecimenKeyForSequenced <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/sequencing_ids_metadata_09132022_updated.csv"
pathTo1718SpecimenKeyForSequenced <- '/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/sequencing_ids_metadata_11042020.csv'
pathToPassCoverageRvtn <- '/Users/katykrupinsky/git/FluTES_bottleneck/03 - Input/pass_coverage_RVTN.csv'
pathToPassCoverageFlutes <- '/Users/katykrupinsky/git/FluTES_bottleneck/03 - Input/Pass_coverage_FluTES.csv'

# 1. Determine which ones we have sequences for --------------------------------

all_cons <- createUnifiedConsensusData() %>%
  dplyr::rename(region = REGION, pos = POS, cons = CONS)

sequenced_ids_df <- all_cons %>%
  ungroup() %>%
  select(sample, strain) %>%
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
  select(sample, strain, sequenced)

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
  select(sample) %>%
  distinct() %>%
  as_vector()

all_cons2 <- all_cons %>%
  filter(sample %in% ids) %>% 
  ungroup() %>% 
  select(sample, strain) %>% 
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
  col.names = TRUE
)

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
