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

list <- list()
list2 <- list()
for (i in 1:nrow(final_pairs)) {
  temp <- final_pairs %>%
    filter(pair_id == i)

  donor_id <- temp$donor_id

  donor <- all_cons %>%
    filter(sample == donor_id) %>%
    left_join(df2) %>%
    select(sample, region, pos, cons, ref, alt) %>%
    dplyr::rename(donor_cons = cons,
                  donor_id = sample,
                  donor_alt = alt) %>%
    select(-ref)

  recipient_id <- temp$recipient_id

  recipient <- all_cons %>%
    filter(sample == recipient_id) %>%
    left_join(df2) %>%
    select(sample, region, pos, cons, ref, alt) %>%
    dplyr::rename(
      recipient_cons = cons,
      recipient_id = sample,
      recipient_alt = alt
    ) %>%
    select(-ref)

  pair <- full_join(donor, recipient) %>%
    filter(is.na(donor_alt) & is.na(recipient_alt) &
             # Filtering out sites that have indels and indeterminate nucleotides
             donor_cons != '-' & recipient_cons != '-' &
             donor_cons != "N" & recipient_cons != "N") %>%
    mutate(clonal_diff = ifelse(donor_cons != recipient_cons, TRUE, FALSE))

  pair_no_filter <- full_join(donor, recipient) %>%
    mutate(clonal_diff = ifelse(donor_cons != recipient_cons &
                                  donor_cons != "N" | donor_cons != "-" &
                                  recipient_cons != "N" | recipient_cons != "-",
                                TRUE, FALSE),
           isnv = ifelse(!is.na(donor_alt) | !is.na(recipient_cons), TRUE, FALSE),
           indel = ifelse(donor_cons == "-" | recipient_cons == "-", TRUE, FALSE),
           indeterminate = ifelse(donor_cons == "N" | recipient_cons == "N", TRUE, FALSE)) %>%
    mutate(pair_id = i)

  out <- temp %>%
    mutate(clonal_diff = sum(pair$clonal_diff))

  list[[i]] <- out
  list2[[i]] <- pair_no_filter
}

clonal_dist_with_meta <- bind_rows(list)
all_sites_pair <- bind_rows(list2)

# # Look at where the thing are ----------------------------------

# full_length <- all_sites_pair %>%
#   ungroup() %>%
#   group_by(pair_id) %>%
#   mutate(concat_pos = row_number()) %>%
#   mutate(isnv = !is.na(donor_alt) | !is.na(recipient_alt),
#          clonal_diff = if_else(donor_cons %in% c("A", "T", "G", "C") &
#                                  recipient_cons %in% c("A", "T", "G", "C") &
#                                  donor_cons != recipient_cons, TRUE, NA)) %>%
#   mutate(isnv = ifelse(isnv == FALSE, NA, TRUE),
#          indeterminate = if_else(indeterminate == FALSE, NA, TRUE),
#          indel = if_else(indel == FALSE, NA, TRUE))
#
# size <- full_length %>% summarise(length = max(concat_pos))
#
#
# From Emily B: "Data visulaization gone bad"
# ggplot() +
#   geom_line(full_length,
#             mapping = aes(x = concat_pos, y = pair_id, group = pair_id, col = region), lwd = 2, alpha = 0.3) +
#   theme_classic() +
#   geom_point(full_length %>% filter(!is.na(indeterminate)), mapping = aes(x = concat_pos, y = pair_id), shape = "-") +
#   geom_point(full_length %>% filter(!is.na(indel)), mapping = aes(x = concat_pos, y = pair_id), shape = 8) +
#   geom_point(full_length %>% filter(!is.na(isnv)), mapping = aes(x = concat_pos, y = pair_id), shape = 19, col = "red") +
#   geom_point(full_length %>% filter(!is.na(clonal_diff)), mapping = aes(x = concat_pos, y = pair_id), shape = 17, col = "blue") +
#   scale_y_continuous(n.breaks = 50, expand = c(0.007, 0.007)) +
#   scale_color_paletteer_d("ggthemes::hc_darkunica") +
#   geom_point(final_pairs, mapping = aes(x = -3, y = pair_id, fill = as.factor(season)), shape = 21, size = 2)

# ## Quick little baby visualization
#
# clonal_dist_with_meta %>%
#   mutate(season = as.character(season)) %>%
#   ggplot(aes(x = clonal_diff)) +
#   geom_histogram(binwidth = 1, boundary = 0, closed = "left", col = "white") +
#   theme_bw(base_size = 24) +
#   scale_fill_paletteer_d(palette = "LaCroixColoR::Pamplemousse") +
#   theme(legend.position = c(0.9, 0.8)) +
#   scale_x_continuous(breaks = seq(0, 80, 1), expand = c(0, 0)) +
#   scale_y_continuous(breaks = seq(0, 64, by = 4), expand = c(0, 0.2)) +
#   labs(x = "Number of Clonal Differences", y = "Frequency", fill = "Season")

# 6. Create the matrix that goes into the actual bottleneck calculation --------
t <- data.frame(clonalMu = 0:max(clonal_dist_with_meta$clonal_diff))

mutation_matrix <- as.data.frame(table(clonal_dist_with_meta$clonal_diff)) %>%
  dplyr::rename(clonalMu = Var1,
                freq = Freq) %>%
  mutate(clonalMu = as.character(clonalMu),
         clonalMu = as.numeric(clonalMu)) %>%
  full_join(t) %>%
  arrange(clonalMu) %>%
  mutate(freq = ifelse(is.na(freq), 0, freq))

# rm(list = setdiff(ls(), c("mutation_matrix", "clonal_dist_with_meta", "createClonalSummary")))

# 7. Create our host and viral co-factor datasets
clonal_dist_with_meta2 <- clonal_dist_with_meta %>%
  mutate(
    subtype = str_extract(strain, "H\\d+N\\d+"),
    season2 = case_when(
      season == 17 ~ "17/18",
      season == 18 ~ "18/19",
      season == 19 ~ "19/20",
      season == 21 ~ "21/22"
    )
  ) %>%
  mutate(
    age_cat = case_when(
      donor_age < 18 & recipient_age < 18 ~ "Child-to-child",
      donor_age < 18 & recipient_age >= 18 ~ "Child-to-adult",
      donor_age >= 18 & recipient_age >= 18 ~ "Adult-to-adult",
      donor_age >= 18 & recipient_age < 18 ~ "Adult-to-child"
    ),
    vax_cat = case_when(
      donor_vax == 0 & recipient_vax == 0 ~ "Neither",
      donor_vax == 0 &
        recipient_vax == 1 ~ "Recipient only",
      donor_vax == 1 & recipient_vax == 0 ~ "Donor only",
      donor_vax == 1 & recipient_vax == 1 ~ "Both"
    ),
    sex_cat = case_when(
      donor_sex == 1 & recipient_sex == 1 ~ "Both Female",
      donor_sex == 1 & recipient_sex == 2 ~ "Female-to-male",
      donor_sex == 2 & recipient_sex == 1 ~ "Male-to-female",
      donor_sex == 2 & recipient_sex == 2 ~ "Both male"
    )
  )

mutation_matrix_17 <- createClonalSummary(clonal_dist_with_meta2, "season2", "17/18")
mutation_matrix_18 <- createClonalSummary(clonal_dist_with_meta2, "season2", "18/19")
mutation_matrix_19 <- createClonalSummary(clonal_dist_with_meta2, "season2", "19/20")
mutation_matrix_21 <- createClonalSummary(clonal_dist_with_meta2, "season2", "21/22")

mutation_matrix_H1N1 <- createClonalSummary(clonal_dist_with_meta2, "subtype", "H1N1")
mutation_matrix_H3N2 <- createClonalSummary(clonal_dist_with_meta2, "subtype", "H3N2")

mutation_matrix_adultToAdult <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Adult-to-adult")
mutation_matrix_adultToChild <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Adult-to-child")
mutation_matrix_childToAdult <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Child-to-adult")
mutation_matrix_childToChild <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Child-to-child")

mutation_matrix_both <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Both")
mutation_matrix_donorOnly <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Donor only")
mutation_matrix_neither <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Neither")
mutation_matrix_recipientOnly <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Recipient only")

mutation_matrix_bothFemale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Both Female")
mutation_matrix_bothMale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Both male")
mutation_matrix_femaleToMale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Female-to-male")
mutation_matrix_maleToFemale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Male-to-female")

# # Put it in a format that is useful for plotting
# clonal_mut_all <- list()
#
# clonal_mut_all[[1]] <- mutation_matrix_17
# clonal_mut_all[[2]] <- mutation_matrix_18
# clonal_mut_all[[3]] <- mutation_matrix_19
# clonal_mut_all[[4]] <- mutation_matrix_21
#
# clonal_mut_all[[5]] <- mutation_matrix_H1N1
# clonal_mut_all[[6]] <- mutation_matrix_H3N2
#
# clonal_mut_all[[7]] <- mutation_matrix_adultToAdult
# clonal_mut_all[[8]] <- mutation_matrix_adultToChild
# clonal_mut_all[[9]] <- mutation_matrix_childToAdult
# clonal_mut_all[[10]] <- mutation_matrix_childToChild
#
# clonal_mut_all[[11]] <- mutation_matrix_both
# clonal_mut_all[[12]] <- mutation_matrix_donorOnly
# clonal_mut_all[[13]] <- mutation_matrix_neither
# clonal_mut_all[[14]] <- mutation_matrix_recipientOnly
#
# clonal_mut_all[[15]] <- mutation_matrix_bothFemale
# clonal_mut_all[[16]] <- mutation_matrix_bothMale
# clonal_mut_all[[17]] <- mutation_matrix_femaleToMale
# clonal_mut_all[[18]] <- mutation_matrix_maleToFemale
#
# clonal_mut_all[[19]] <- mutation_matrix %>%
#   mutate(level = "Overall")
#
# clonal_mut_all <- bind_rows(clonal_mut_all)
#
# write.csv(clonal_mut_all, file = '/Users/katykrupinsky/git/FluTES_bottleneck/04 - Output/clonal_dist_by_factor.csv')
#
# num_in <- clonal_mut_all %>%
#   group_by(level) %>%
#   summarise(num = sum(freq))
#
# write.csv(num_in, file = '/Users/katykrupinsky/git/FluTES_bottleneck/04 - Output/clonal_num_samples.csv')


