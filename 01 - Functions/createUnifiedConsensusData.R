createUnifiedConsensusData <- function(includeRvtn = TRUE) {
  # 1. Setup -------------------------------------------------------------------
  source("01 - Functions/getConsensus.R")
  
  brisbane_len <- "03 - Input/Brisbane_length_to_add_new.csv"
  brisbane_fasta <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Brisbane_H1N1_2019_aligned.all.consensus.fasta"
  singapore_len <- "~/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Singapore_length_to_add.csv"
  singapore_fasta <- "~/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Singapore_H3N2_2018.all.consensus_new.fasta"
  darwin_fasta <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Darwin_H3N2_2021.all.consensus.fasta"
  hongkong_pos <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/HongKong_all.consensus_positions"
  michigan_pos <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Michigan_H1N1_2017_2018.all.consensus_positions"
  
  list <- list()
  
  # 2. Brisbane_H1N1_2019 ------------------------------------------------------
  Brisbane_H1N1_2019 <- getConsensus(
    path_to_length_file = brisbane_len,
    path_to_fasta_file = brisbane_fasta,
    ref_name = "Brisbane_ref",
    strain_name = "Brisbane_H1N1_2019",
    column_with_lengths = "Length_to_add_new"
  )
  
  list[[1]] <- Brisbane_H1N1_2019
  
  # 3. Singapore_H3N2_2018 -----------------------------------------------------
  Singapore_H3N2_2018 <- getConsensus(
    path_to_length_file = singapore_len,
    path_to_fasta_file = singapore_fasta,
    ref_name = "Reference",
    strain_name = "Singapore_H3N2_2018",
    column_with_lengths = "Length_to_add"
  )
  
  list[[2]] <- Singapore_H3N2_2018
  
  # 4. Darwin_H3N2_2021 --------------------------------------------------------
  if(includeRvtn == TRUE){
  Darwin_H3N2_2021 <- phylotools::read.fasta(darwin_fasta) %>%
    ungroup() %>%
    dplyr::rename(sample = seq.name, CONS = seq.text) %>%
    mutate(strain = "Darwin_H3N2_2021", sample = as.numeric(sample)) %>%
    group_by(sample) %>%
    mutate(REGION = c("PB2", "PB1", "PA", "HA", "NP", "NA_", "M", "NS")) %>%
    mutate(CONS = str_split(CONS, "(?=.)")) %>%
    unnest(cols = c(CONS)) %>%
    filter(CONS != "") %>%
    ungroup() %>%
    group_by(sample, REGION) %>%
    mutate(POS = row_number()) %>%
    select(sample, REGION, POS, CONS, strain)
  
  list[[3]] <- Darwin_H3N2_2021
  }
  
  # 5. HongKong_H3N2_2017 ------------------------------------------------------
  HongKong_H3N2_2017 <- read.csv(hongkong_pos) %>%
    dplyr::rename(CONS = consensus_allele, REGION = CHROM) %>%
    mutate(strain = "HongKong_H3N2_2017") %>% 
    select(sample, REGION, POS, CONS, strain) 
  
  list[[4]] <- HongKong_H3N2_2017
  
  # 6. Michian_H1N1_2017 -------------------------------------------------------
  Michigan_H1N1_2017 <- read.csv(michigan_pos) %>%
    dplyr::rename(CONS = consensus_allele, REGION = CHROM) %>%
    mutate(strain = "Mighigan_H1N1_2017") %>% 
    select(sample, REGION, POS, CONS, strain) 
  
  list[[5]] <- Michigan_H1N1_2017
  
  # 7. Final join --------------------------------------------------------------
  all_cons <- bind_rows(list)
  
  return(all_cons)
}