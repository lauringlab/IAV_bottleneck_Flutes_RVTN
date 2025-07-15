# Install Biostrings if you haven't already
if (!requireNamespace("Biostrings", quietly = TRUE)) {
  BiocManager::install("Biostrings")
}
library(Biostrings)

# Load aligned FASTA file
fasta_file <- "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/treeData/02 - singapore_2018/aligned_seqs.fasta"  # replace with your actual file path
aligned_seqs <- readDNAStringSet(fasta_file)

# Define your list of ID pairs as a data frame
# pair_list <- data.frame(
#   id1 = c("sample_1", "sample_3", "sample_5"),
#   id2 = c("sample_2", "sample_4", "sample_6"),
#   stringsAsFactors = FALSE
# )

pair_list <- bottleneck_meta2 %>% 
  filter(strain == "Singapore_H3N2_2018") %>% 
  dplyr::select(donor_id, recipient_id) %>% 
  dplyr::rename(id1 = donor_id,
                id2 = recipient_id) %>% 
  dplyr::mutate(id1 = as.character(id1),
                id2 = as.character(id2))

# Function to calculate mutation count between two sequences
count_mutations <- function(id1, id2, sequences) {
  seq1 <- as.character(sequences[[id1]])
  seq2 <- as.character(sequences[[id2]])
  
  if (is.null(seq1) || is.null(seq2)) {
    warning(paste("Missing ID:", id1, "or", id2))
    return(NA)
  }
  
  if (nchar(seq1) != nchar(seq2)) {
    stop(paste("Sequences", id1, "and", id2, "are not the same length"))
  }
  
  sum(substring(seq1, 1:nchar(seq1), 1:nchar(seq1)) != 
        substring(seq2, 1:nchar(seq2), 1:nchar(seq2)))
}

# Apply the function to all pairs
pair_list$mutation_count <- mapply(count_mutations, 
                                   id1 = pair_list$id1, 
                                   id2 = pair_list$id2, 
                                   MoreArgs = list(sequences = aligned_seqs))


count_mutations(id1 = pair_list$id1[1],
                id2 = pair_list$, 
                sequences = aligned_seqs)

# View result
print(pair_list)


pair_list %>% 
  ggplot(aes(x = mutation_count)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal(base_size = 18) +
  labs(x = "Number of mutations different", y = "Frequency")


