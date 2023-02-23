## Title: Data Processing for Bottleneck Calculations
## Author: Katy Krupinsky
## Date Last Modified: 01/24/23
## Description: This script takes the data from the sampling scheme script and
## processes it into the format needed for input in the bottleneckCalculation.R
## script.
##
## -----------------------------------------------------------------------------

## Path names - to be updated depending on local location of data --------------

path_to_raw_data <- "./imput/IAV_meta_snv.csv"

path_to_sampleScheme <- "./FluTES_bottleneck/scripts/00-samplingScheme_onsetDate.R"
path_to_consensusSequences <- "./FluTES_bottleneck/scripts/00-consensusSequences.R"

## Load packages, raw data, and source script ----------------------------------
library(tidyverse)
library(lubridate)

source(path_to_sampleScheme)
source(path_to_consensusSequences)
# rm(list=setdiff(ls(), c("all_sequences", "output_dataset")))

raw_data <- read.csv(path_to_raw_data)

## Get data into form necessary for bottleneck calculation ---------------------
## -----------------------------------------------------------------------------

df <- output_dataset %>% 
    mutate(index2 = ifelse(index == 0, 2, 1))
rm(output_dataset)

# Do more work to get the samples arranged by onset date                     ---

donor_recipient <- df %>% 
    select(subj_id, year, household, onset_date, collection_date,
           index2, full_id) %>% 
    distinct() %>% 
    mutate(onset_date = ifelse(is.na(onset_date),
                               collection_date, onset_date)) %>% 
    select(-collection_date) %>% 
    mutate(onset_date = as.Date(onset_date, origin)) %>% 
    arrange(year, household, onset_date, index2) %>% 
    group_by(year, household) %>% 
    mutate(sample_order = row_number()) %>% 
    mutate(type = ifelse(sample_order == 1, "donor",
                         ifelse(sample_order == 2,
                                "recipient_1", "recipient_2"))) %>% 
    ungroup() %>% 
    select(year, household, type, full_id) %>% 
    spread(type, full_id) %>% 
    select(year, household, donor, recipient_1, recipient_2) %>% 
    gather(x, recipient, -c(year, household, donor)) %>% 
    na.omit() %>% 
    arrange(year, household) %>% 
    select(-x) %>% 
    mutate(pair_id = row_number())

# Pull out donor and recipient ids                                           ---
donor_ids <- donor_recipient %>% 
    select(pair_id, donor)

recipient_ids <- donor_recipient %>% 
    select(pair_id, recipient)

# Pull out exact mutations for our donor and recipient ids                   ---
donor_mutations <- donor_ids %>% 
    rename(full_id = donor) %>% 
    inner_join(df, multiple = "all") %>% 
    select(pair_id, full_id, year, household, genome_segment, segment_position,
           ref_allele, alt_allele, avg_freq) %>% 
    rename(donor_ref_allele = ref_allele, 
           donor_alt_allele = alt_allele,
           donor_freq = avg_freq,
           donor_id = full_id)

recipient_mutations <- recipient_ids %>% 
    rename(full_id = recipient) %>% 
    inner_join(df, multiple = "all") %>% 
    select(pair_id, full_id, year, household, genome_segment, segment_position,
           ref_allele, alt_allele, avg_freq) %>% 
    rename(recipient_ref_allele = ref_allele, 
           recipient_alt_allele = alt_allele,
           recipient_freq = avg_freq,
           recipient_id = full_id)

## Look up the consensus alleles are for each of these samples -----------------

# Get our final list of ids                                                  ---
final_ids <- df %>%
    select(full_id) %>%
    distinct() %>% 
    as_vector()

# Get our sequence data into a usable format                                ---
final_sequences_wide <- all_sequences %>% 
    subset(full_id %in% final_ids)
final_sequences_long <- final_sequences_wide %>% 
    gather(position, base, -c(full_id, genome_segment)) %>% 
    na.omit() %>% 
    rename(segment_position = position) %>% 
    mutate(segment_position = as.integer(segment_position))

# Create data frame with donor-recipient pairs and mutations matched         ---
df2 <- full_join(donor_mutations, recipient_mutations) %>% 
    full_join(donor_recipient, multiple = "all") %>% 
    select(pair_id, donor, recipient, year, household, genome_segment,
           segment_position, donor_ref_allele, donor_alt_allele, donor_freq,
           recipient_ref_allele, recipient_alt_allele, recipient_freq)

# Find the consensus allele for each sample at locations of interest         ---
donor_consensus <- df2 %>% 
    select(pair_id, donor, year, household, genome_segment, segment_position,
           donor_ref_allele) %>% 
    rename(full_id = donor) %>% 
    left_join(final_sequences_long) %>% 
    rename(donor_cons_allele = base,
           donor = full_id)

recipient_consensus <- df2 %>% 
    select(pair_id, recipient, year, household, genome_segment,
           segment_position, recipient_ref_allele) %>% 
    rename(full_id = recipient) %>% 
    left_join(final_sequences_long) %>% 
    rename(recipient_cons_allele = base,
           recipient = full_id)

# Put it all together in a single data frame                                 ---
df3 <- df2 %>% 
    full_join(recipient_consensus) %>% 
    full_join(donor_consensus) %>% 
    select(pair_id, donor, recipient, year, household,
           genome_segment, segment_position,
           donor_ref_allele, donor_cons_allele, 
           donor_alt_allele, donor_freq,
           recipient_ref_allele, recipient_cons_allele, 
           recipient_alt_allele, recipient_freq) 

# Alter frequency readings according to consensus and alternative alleles    ---
### Define the conditions for future filtering
df4 <- df3 %>% 
    mutate(only_donor = ifelse(is.na(recipient_freq), TRUE, FALSE),
           only_recipient = ifelse(is.na(donor_freq), TRUE, FALSE),
           both = ifelse(!is.na(donor_freq) & !is.na(recipient_freq),
                         TRUE, FALSE),
           
           same_ref = ifelse(donor_ref_allele == recipient_ref_allele,
                             TRUE, FALSE),
           same_alt = ifelse(donor_alt_allele == recipient_alt_allele,
                             TRUE, FALSE),
           same_cons = ifelse(donor_cons_allele == recipient_cons_allele,
                              TRUE, FALSE),
           dCons_rAlt = ifelse(donor_cons_allele == recipient_alt_allele,
                               TRUE, FALSE),
           dAlt_rCons = ifelse(donor_alt_allele == recipient_cons_allele,
                               TRUE, FALSE))

### Make the necessary changes
df5 <- df4 %>% 
    mutate(donor_freq2 = ifelse(both == TRUE,
                                ifelse(same_cons == TRUE, donor_freq,
                                       ifelse(dCons_rAlt == TRUE,
                                              donor_freq, NA)),
                                ifelse(only_donor == TRUE,
                                       ifelse(same_cons == TRUE,
                                              (1 - donor_freq),
                                              ifelse(dAlt_rCons == TRUE,
                                                     donor_freq, NA)),
                                       ifelse(same_cons == TRUE,
                                              1,
                                              ifelse(dCons_rAlt == TRUE,
                                                     1, NA)))),
           recipient_freq2 = ifelse(both == TRUE,
                                    ifelse(same_cons == TRUE, recipient_freq,
                                           ifelse(dCons_rAlt == TRUE,
                                                  (1-recipient_freq), NA)),
                                    ifelse(only_donor == TRUE,
                                           ifelse(same_cons == TRUE, 1,
                                                  ifelse(dAlt_rCons == TRUE,
                                                         1, NA)),
                                           ifelse(same_cons == TRUE,
                                                  (1-recipient_freq),
                                                  ifelse(dCons_rAlt == TRUE,
                                                         recipient_freq, NA)))))
pair_meta <- df5 %>%
    filter(only_recipient != TRUE) %>%
    select(pair_id, donor, recipient, year, household) %>%
    distinct()

write.table(pair_meta,
            file = "./input/pair_meta_unfiltered.txt",
            sep = "\t",
            row.names = FALSE, col.names = TRUE)
