createIndividualSampleSegmentFiles <- function() {
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
  
  ids <- Brisbane_H1N1_2019 %>% ungroup() %>% select(sample) %>% distinct()
  
  for (i in 1:nrow(ids)) {
    temp <- Brisbane_H1N1_2019 %>%
      filter(sample == as.numeric(ids[i, ])) %>%
      mutate(
        lev = case_when(
          REGION == "PB2" ~ 1,
          REGION == "PB1" ~ 2,
          REGION == "PA" ~ 3,
          REGION == "HA" ~ 4,
          REGION == "NP" ~ 5,
          REGION == "NA_" ~ 6,
          REGION == "M" ~ 7,
          REGION == "NS" ~ 8
        )
      ) %>%
      arrange(lev) %>%
      group_by(sample, REGION, lev) %>%
      summarize(seq = paste(CONS, collapse = "")) %>%
      arrange(sample, lev) %>%
      mutate(length = str_length(seq)) %>%
      ungroup() %>%
      select(REGION, seq) %>%
      dplyr::rename(seq.name = REGION, seq.text = seq)
    
    filename <- paste(
      "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/segmentFasta/Brisbane/",
      as.character(ids[i, ]),
      ".fasta",
      sep = ""
    )
    
    phylotools::dat2fasta(temp, outfile = filename)
  }
  
  # 3. Singapore_H3N2_2018 -----------------------------------------------------
  Singapore_H3N2_2018 <- getConsensus(
    path_to_length_file = singapore_len,
    path_to_fasta_file = singapore_fasta,
    ref_name = "Reference",
    strain_name = "Singapore_H3N2_2018",
    column_with_lengths = "Length_to_add"
  )
  
  ids <- Singapore_H3N2_2018 %>% ungroup() %>% select(sample) %>% distinct()
  
  for (i in 1:nrow(ids)) {
    temp <- Singapore_H3N2_2018 %>%
      filter(sample == as.numeric(ids[i, ])) %>%
      mutate(
        lev = case_when(
          REGION == "PB2" ~ 1,
          REGION == "PB1" ~ 2,
          REGION == "PA" ~ 3,
          REGION == "HA" ~ 4,
          REGION == "NP" ~ 5,
          REGION == "NA_" ~ 6,
          REGION == "M" ~ 7,
          REGION == "NS" ~ 8
        )
      ) %>%
      arrange(lev) %>%
      group_by(sample, REGION, lev) %>%
      summarize(seq = paste(CONS, collapse = "")) %>%
      arrange(sample, lev) %>%
      mutate(length = str_length(seq)) %>%
      ungroup() %>%
      select(REGION, seq) %>%
      dplyr::rename(seq.name = REGION, seq.text = seq)
    
    filename <- paste(
      "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/segmentFasta/Singapore/",
      as.character(ids[i, ]),
      ".fasta",
      sep = ""
    )
    
    phylotools::dat2fasta(temp, outfile = filename)
  }
  
  # 4. Darwin_H3N2_2021 --------------------------------------------------------
  if (includeRvtn == TRUE) {
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
      dplyr::select(sample, REGION, POS, CONS, strain)
    
    ids <- Darwin_H3N2_2021 %>% ungroup() %>% select(sample) %>% distinct()
    
    for (i in 1:nrow(ids)) {
      temp <- Darwin_H3N2_2021 %>%
        filter(sample == as.numeric(ids[i, ])) %>%
        mutate(
          lev = case_when(
            REGION == "PB2" ~ 1,
            REGION == "PB1" ~ 2,
            REGION == "PA" ~ 3,
            REGION == "HA" ~ 4,
            REGION == "NP" ~ 5,
            REGION == "NA_" ~ 6,
            REGION == "M" ~ 7,
            REGION == "NS" ~ 8
          )
        ) %>%
        arrange(lev) %>%
        group_by(sample, REGION, lev) %>%
        summarize(seq = paste(CONS, collapse = "")) %>%
        arrange(sample, lev) %>%
        mutate(length = str_length(seq)) %>%
        ungroup() %>%
        select(REGION, seq) %>%
        dplyr::rename(seq.name = REGION, seq.text = seq)
      
      filename <- paste(
        "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/segmentFasta/Darwin/",
        as.character(ids[i, ]),
        ".fasta",
        sep = ""
      )
      
      phylotools::dat2fasta(temp, outfile = filename)
    }
  }
  
  # 5. HongKong_H3N2_2017 ------------------------------------------------------
  HongKong_H3N2_2017 <- read.csv(hongkong_pos) %>%
    dplyr::rename(CONS = consensus_allele, REGION = CHROM) %>%
    mutate(strain = "HongKong_H3N2_2017") %>%
    dplyr::select(sample, REGION, POS, CONS, strain)
  
  ids <- HongKong_H3N2_2017 %>% ungroup() %>% select(sample) %>% distinct()
  
  for (i in 1:nrow(ids)) {
    temp <- HongKong_H3N2_2017 %>%
      filter(sample == as.numeric(ids[i, ])) %>%
      mutate(
        lev = case_when(
          REGION == "PB2" ~ 1,
          REGION == "PB1" ~ 2,
          REGION == "PA" ~ 3,
          REGION == "HA" ~ 4,
          REGION == "NP" ~ 5,
          REGION == "NA_" ~ 6,
          REGION == "M" ~ 7,
          REGION == "NS" ~ 8
        )
      ) %>%
      arrange(lev) %>%
      group_by(sample, REGION, lev) %>%
      summarize(seq = paste(CONS, collapse = "")) %>%
      arrange(sample, lev) %>%
      mutate(length = str_length(seq)) %>%
      ungroup() %>%
      select(REGION, seq) %>%
      dplyr::rename(seq.name = REGION, seq.text = seq)
    
    filename <- paste(
      "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/segmentFasta/HongKong/",
      as.character(ids[i, ]),
      ".fasta",
      sep = ""
    )
    
    phylotools::dat2fasta(temp, outfile = filename)
  }
  
  # 6. Michian_H1N1_2017 -------------------------------------------------------
  Michigan_H1N1_2017 <- read.csv(michigan_pos) %>%
    dplyr::rename(CONS = consensus_allele, REGION = CHROM) %>%
    mutate(strain = "Mighigan_H1N1_2017") %>%
    dplyr::select(sample, REGION, POS, CONS, strain)
  
  ids <- Michigan_H1N1_2017 %>% ungroup() %>% select(sample) %>% distinct()
  
  for (i in 1:nrow(ids)) {
    temp <- Michigan_H1N1_2017 %>%
      filter(sample == as.numeric(ids[i, ])) %>%
      mutate(
        lev = case_when(
          REGION == "PB2" ~ 1,
          REGION == "PB1" ~ 2,
          REGION == "PA" ~ 3,
          REGION == "HA" ~ 4,
          REGION == "NP" ~ 5,
          REGION == "NA_" ~ 6,
          REGION == "M" ~ 7,
          REGION == "NS" ~ 8
        )
      ) %>%
      arrange(lev) %>%
      group_by(sample, REGION, lev) %>%
      summarize(seq = paste(CONS, collapse = "")) %>%
      arrange(sample, lev) %>%
      mutate(length = str_length(seq)) %>%
      ungroup() %>%
      select(REGION, seq) %>%
      dplyr::rename(seq.name = REGION, seq.text = seq)
    
    filename <- paste(
      "/Users/katykrupinsky/University of Michigan Dropbox/Katy Krupinsky/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/segmentFasta/Michigan/",
      as.character(ids[i, ]),
      ".fasta",
      sep = ""
    )
    
    phylotools::dat2fasta(temp, outfile = filename)
  }
}
