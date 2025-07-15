## Title: Creation of Input Files for Bottleneck Calculations - Clonal Method with edges filtered
## Date Last Modified: 03/25/25
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
source("./06 - Clonal Method Code/01 - Functions/createClonalDataByCoFactors.R")
rm(files.sources)
path_to_fulldat <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/CDC_RVTN_FLUTES_Flu/CDC_RVTN_FLUTES_Flu_2/fulldat_lauring_flu2023-12-07.csv"
path_to_ddlabdat <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/CDC_RVTN_FLUTES_Flu/CDC_RVTN_FLUTES_Flu_2/ddlabdat_lauring_flu2023-12-07.csv"
path_to_flutes_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Flutes/SNV_with_meta_data.csv"
path_to_rvtn_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/RVTN/SNV_with_mutation_type.csv"
path_to_flutes_meta <- "~/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/Vanderbilt_metadata_all_years.csv"
pathTo19SpecimenKeyForSequenced <- "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/sequencing_ids_metadata_09132022_updated.csv"
pathTo1718SpecimenKeyForSequenced <- '/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Flutes/sequencing_ids_metadata_11042020.csv'
pathToPassCoverageRvtn <- '/Users/katykrupinsky/git/FluTES_bottleneck/03 - Input/pass_coverage_rvtn.csv'
pathToPassCoverageFlutes <- '/Users/katykrupinsky/git/FluTES_bottleneck/03 - Input/pass_coverage_flutes.csv'

# 1. Determine which ones we have sequences for --------------------------------

all_cons <- createUnifiedConsensusData() %>%
  dplyr::rename(region = REGION, pos = POS, cons = CONS)

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

# 3. Clean data ----------------------------------------------------------------
df2 <- cleanUnifiedDataset(df, filter = TRUE, nSnvFilterThresh = 50)

# 4. Create transmission pairs -------------------------------------------------
final_pairs <- determineTransmissionPairs_clonal(df2)

#5. Combine with consensus and iSNV information and get the number of
#   clonal mutations -----------------------------------------------------------

edges <- all_cons %>% 
  group_by(sample, region, strain) %>% 
  summarise(start = min(pos),
            end = max(pos)) %>% 
  ungroup() %>% 
  arrange(strain, region) %>% 
  select(-sample) %>% 
  distinct()

list <- list()
list2 <- list()
for (i in 1:nrow(final_pairs)) {
  temp <- final_pairs %>%
    filter(pair_id == i)
  
  edge_strain <- edges %>% 
    filter(strain == temp$strain)
  
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
    full_join(edge_strain) %>% 
    dplyr::filter(pos > start + 20 & pos < end - 20)
  
  
  
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
    full_join(edge_strain) %>% 
    dplyr::filter(pos > start + 50 & pos < end - 50)
  
  out <- temp %>%
    mutate(clonal_diff = sum(pair$clonal_diff))
  
  
  # # This is the line that filters based on a threshhold of number of clonal differences, comment out lines 153-155 & 158 if you don't want to filter actually
  # if (out$clonal_diff >= 5) {
  #   
  # } else {
  list[[i]] <- out
  list2[[i]] <- pair_no_filter
  # }
}

clonal_dist_with_meta <- bind_rows(list)
all_sites_pair <- bind_rows(list2)

write.table(
  clonal_dist_with_meta,
  file = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_dist_with_meta_50edge.txt",
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

write_csv(clonal_mut_all, file = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_mut_all_50edge.csv')

rm(list = setdiff(ls(), c("clonal_mut_all", "listClonal")))
