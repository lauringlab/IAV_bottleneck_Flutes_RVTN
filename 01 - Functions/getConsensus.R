getConsensus <- function(path_to_length_file,
                           path_to_fasta_file,
                           ref_name,
                           strain_name,
                           column_with_lengths) {
  segments <- c(
    "PB2" = 1,
    "PB1" = 2,
    "PA" = 3,
    "HA" = 4,
    "NP" = 5,
    "NA_" = 6,
    "M" = 7,
    "NS" = 8
  )
  
  length_to_add <- read_csv(path_to_length_file) %>%
    dplyr::select(column_with_lengths) %>%
    as_vector()
  
  out <- phylotools::read.fasta(path_to_fasta_file) %>%
    as_tibble() %>% 
    dplyr::rename(sample = seq.name, CONS = seq.text) %>%
    filter(sample != ref_name) %>%
    mutate(CONS = str_split(CONS, "(?=.)")) %>%
    unnest(cols = c(CONS)) %>%
    filter(CONS != "") %>%
    mutate(sample = as.numeric(sample),
           CONS = toupper(CONS),
           strain = strain_name) %>%
    group_by(sample) %>%
    mutate(pos_adj = row_number()) %>%
    mutate(
      REGION = case_when(
        pos_adj > length_to_add[[unname(segments["NS"])]] ~ "NS",
        pos_adj > length_to_add[[unname(segments["M"])]] ~ "M",
        pos_adj > length_to_add[[unname(segments["NA_"])]] ~ "NA_",
        pos_adj > length_to_add[[unname(segments["NP"])]] ~ "NP",
        pos_adj > length_to_add[[unname(segments["HA"])]] ~ "HA",
        pos_adj > length_to_add[[unname(segments["PA"])]] ~ "PA",
        pos_adj > length_to_add[[unname(segments["PB1"])]] ~ "PB1",
        pos_adj > length_to_add[[unname(segments["PB2"])]] ~ "PB2",
        .default = "Other"
      )
    ) %>%
    group_by(sample, REGION) %>%
    mutate(POS = row_number()) %>%
    dplyr::select(sample, REGION, POS, CONS, strain)
  
  return(out)
}
