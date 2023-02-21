## Title: Processing of Consensus Sequence Data
## Author: Katy Krupinsky
## Date Last Modified: 01/24/23
## Description: This script processes the consensus sequence data for use in
## dependent scripts.
##
## -----------------------------------------------------------------------------

HongKong <- read.csv("./consensusSequences/HongKong_all.consensus_positions")

HongKong_clean <- HongKong %>% 
    rename(genome_segment = CHROM,
           segment_position = POS,
           ref_allele = consensus_allele,
           full_id = sample)

HongKong_wide <- HongKong_clean %>% 
    spread(segment_position, ref_allele)

HongKong_ids <- HongKong_wide %>% 
    select(full_id) %>% 
    distinct() %>% 
    mutate(strain = "HongKong")

Michigan_2017 <- read.csv("./consensusSequences/Michigan_H1N1_2017_2018.all.consensus_positions")

Michigan_2017_clean <- Michigan_2017 %>% 
    rename(genome_segment = CHROM,
           segment_position = POS,
           ref_allele = consensus_allele,
           full_id = sample)

Michigan_2017_wide <- Michigan_2017_clean %>% 
    spread(segment_position, ref_allele)

Michigan_2017_ids <- Michigan_2017_wide %>% 
    select(full_id) %>% 
    distinct() %>% 
    mutate(strain = "Michigan2017")

Michigan_2018 <- read.csv("./consensusSequences/Sigapore_H3N2_2018.all.consensus_positions")

Michigan_2018_clean <- Michigan_2018 %>% 
    rename(genome_segment = CHROM,
           segment_position = POS,
           ref_allele = consensus_allele,
           full_id = sample)

Michigan_2018_wide <- Michigan_2018_clean %>% 
    spread(segment_position, ref_allele)

Michigan_2018_ids <- Michigan_2018_wide %>% 
    select(full_id) %>% 
    distinct() %>% 
    mutate(strain = "Michigan2018")

all_ids_by_strain <- bind_rows(HongKong_ids, Michigan_2017_ids) %>% 
    bind_rows(Michigan_2018_ids)

all_sequences <- bind_rows(HongKong_wide, Michigan_2017_wide) %>% 
    bind_rows(Michigan_2018_wide)
