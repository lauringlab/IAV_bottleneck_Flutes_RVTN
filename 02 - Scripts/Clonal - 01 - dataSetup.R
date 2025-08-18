## Title: Creation of Input Files for Bottleneck Calculations - Clonal Method
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
source("./01 - Functions/createClonalDataByCoFactors.R")
source("./01 - Functions/createUnifiedConsensusDataTrimmed.R")
source("./01 - Functions/joinFlutesSnvToMeta_new.R")
source("./01 - Functions/joinRvtnSnvToMeta_new.R")
source("./01 - Functions/getRvtnMetadata.R")
source("./01 - Functions/cleanUnifiedDataset.R")
source("./01 - Functions/determineTransmissionPairs_clonal.R")
source("./01 - Functions/createClonalSummary.R")

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

# 2. Data import ---------------------------------------------------------------
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

temp <- df %>% 
  select(sample, hhid, cdc_hhid) %>% distinct() %>% 
  mutate(season = ifelse(is.na(hhid), str_sub(sample, 1, 2), 21)) %>% 
  mutate(hhid = ifelse(is.na(hhid), str_sub(sample, 3, 5), hhid)) %>% 
  select(season, hhid) %>% distinct()

# 3. Clean data ----------------------------------------------------------------
df2 <- cleanUnifiedDataset(df, filter = TRUE, nSnvFilterThresh = 50)

# 4. Create transmission pairs -------------------------------------------------
final_pairs <- determineTransmissionPairs_clonal(df2)

# 5. Combine with consensus and iSNV information and get the number of
#   clonal mutations -----------------------------------------------------------
list <- list()
list2 <- list()
for (i in 1:nrow(final_pairs)) {
  temp <- final_pairs %>%
    filter(pair_id == i)
  
  donor_id <- temp$donor_id
  
  donor <- all_cons %>%
    filter(sample == donor_id) %>%
    left_join(df2) %>%
    dplyr::select(sample, region, pos, cons, ref, alt) %>%
    dplyr::rename(donor_cons = cons,
                  donor_id = sample,
                  donor_alt = alt) %>%
    dplyr::select(-ref)
  
  recipient_id <- temp$recipient_id
  
  recipient <- all_cons %>%
    filter(sample == recipient_id) %>%
    left_join(df2) %>%
    dplyr::select(sample, region, pos, cons, ref, alt) %>%
    dplyr::rename(
      recipient_cons = cons,
      recipient_id = sample,
      recipient_alt = alt
    ) %>%
    dplyr::select(-ref)
  
  pair <- full_join(donor, recipient) %>%
    filter(
      is.na(donor_alt) & is.na(recipient_alt) &
        # Filtering out sites that have indels and indeterminate nucleotides
        donor_cons != '-' & recipient_cons != '-' &
        donor_cons != "N" & recipient_cons != "N"
    ) %>%
    mutate(clonal_diff = ifelse(donor_cons != recipient_cons, TRUE, FALSE)) %>% 
    mutate(strain = temp$strain)
  
  pair_no_filter <- full_join(donor, recipient) %>%
    mutate(
      clonal_diff = ifelse(
        donor_cons != recipient_cons &
          donor_cons != "N" &
          donor_cons != "-" &
          recipient_cons != "N" &
          recipient_cons != "-",
        TRUE,
        FALSE
      ),
      isnv = ifelse(!is.na(donor_alt) |
                      !is.na(recipient_cons), TRUE, FALSE),
      indel = ifelse(donor_cons == "-" |
                       recipient_cons == "-", TRUE, FALSE),
      indeterminate = ifelse(donor_cons == "N" |
                               recipient_cons == "N", TRUE, FALSE)
    ) %>%
    mutate(pair_id = i) %>% 
    mutate(strain = temp$strain)
  
  out <- temp %>%
    mutate(clonal_diff = sum(pair$clonal_diff))

    list[[i]] <- out
    list2[[i]] <- pair_no_filter
    
}

clonal_dist_with_meta <- bind_rows(list)
all_sites_pair <- bind_rows(list2)

write.table(
  clonal_dist_with_meta,
  file = "./04 - Output/Clonal_data/clonal_dist_with_meta.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

write.table(
  all_sites_pair,
  file = "./04 - Output/Clonal_data/all_sites_pair.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

# 6. Create our overall + host/viral co-factor data sets -----------------------

t <- data.frame(clonalMu = 0:max(clonal_dist_with_meta$clonal_diff))

mutation_matrix <- as.data.frame(table(clonal_dist_with_meta$clonal_diff)) %>%
  dplyr::rename(clonalMu = Var1, freq = Freq) %>%
  mutate(clonalMu = as.character(clonalMu),
         clonalMu = as.numeric(clonalMu)) %>%
  full_join(t) %>%
  arrange(clonalMu) %>%
  mutate(freq = ifelse(is.na(freq), 0, freq))

clonal_mut_all <- createClonalDataByCoFactors(clonal_dist_with_meta = clonal_dist_with_meta,
                                              saveForPlotting = FALSE)

write_csv(clonal_mut_all, file = './04 - Output/Clonal_data/clonal_mut_all.csv')

rm(list = setdiff(ls(), c()))
