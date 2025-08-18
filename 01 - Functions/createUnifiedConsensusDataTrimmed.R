createUnifiedConsensusDataTrimmed <- function() {
  strains <- c(
    "Brisbane_H1N1_2019",
    "Darwin_H3N2_2021",
    "Michigan_H1N1_2017",
    "Singapore_H3N2_2018"
  )
  list2 <- list()
  
  for (s in 1:length(strains)) {
    country <- sub("_.*", "", strains[[s]])
    
    samples <- list.files(
      path = paste(
        "./03 - Input/trimmedConsensusFiles/trimmed",
        country,
        sep = ""
      ),
      pattern = "*.fasta"
    ) %>%
      as_tibble() %>%
      mutate(value = str_remove(value, ".fasta")) %>%
      as_vector()
    
    list <- list()
    
    for (i in 1:length(samples)) {
      sample <- samples[i]
      
      temp <- phylotools::read.fasta(
        paste(
          "./03 - Input/trimmedConsensusFiles/trimmed",
          country,
          "/",
          sample,
          ".fasta",
          sep = ""
        )
      ) %>%
        ungroup() %>%
        dplyr::rename(segment = seq.name, cons = seq.text) %>%
        mutate(region = c("PB2", "PB1", "PA", "HA", "NP", "NA_", "M", "NS")) %>%
        mutate(cons = str_split(cons, "(?=.)")) %>%
        unnest(cols = c(cons)) %>%
        filter(cons != "") %>%
        ungroup() %>%
        mutate(start = as.numeric(str_extract(segment, "(?<=:)[0-9]{1,2}(?=-)"))) %>%
        group_by(region) %>%
        mutate(pos_raw = row_number()) %>%
        mutate(pos = pos_raw + (start - 1)) %>%
        mutate(sample = sample, strain = strains[[s]]) %>%
        select(sample, region, pos, cons, strain)
      
      list[[i]] <- temp
      
    }
    
    list <- bind_rows(list)
    
    list2[[s]] <- list
  }
  
  
  all_cons2 <- bind_rows(list2)
}