library(tidyverse)
library(argparse)

# 0. Setup ---------------------------------------------------------------------
### Bring in functions ###
file.sources = list.files(
  c("./01 - Functions/bottleneckCalculation_111124"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)
sapply(file.sources, source, .GlobalEnv)
rm(file.sources)

### Setup importing of pairs iSNVs ###
files <- list.files("./03 - Input/processedData") %>%
  data.frame() %>%
  mutate(
    pair = str_replace(., "pair_", ""),
    pair = str_replace(pair, ".txt", ""),
    pair = as.numeric(pair)
  ) %>%
  summarize(max(pair)) %>%
  as.numeric()

### Necessary data frames for analysis ###
pairs <- seq(1, files)
num_vars <- data.frame(pair = 1:length(pairs),
                       # bool = NA,
                       n_variants = 0)
confidence <- data.frame(
  pair = 1:length(pairs),
  max_LL = 000,
  lower_CI = 000,
  upper_CI = 000,
  corrected = 99
)
warnings <- tibble()
ll_out <- list()

### Begin loop ###
for (p in 1:max(pairs)) {
  filename <- paste("./03 - Input/processedData/",
                    paste(paste("pair", p, sep = "_"), ".txt", sep = ""),
                    sep = "")
  
  # Handle command line arguments ------------------------------------------------
  args <- handleCommandLineArgs (
    filename = filename,
    plot_bool = FALSE,
    var_thresh = 0.005,
    nb_min = 1,
    nb_max = 200,
    nb_increment = 1,
    conf_level = 0.95
  )
  
  # Create necessary variables and data frames -----------------------------------
  var_calling_threshold  <- args$var_calling_threshold
  Nb_min <- args$Nb_min
  if (Nb_min < 1) {
    Nb_min = 1
  }
  Nb_max <-  args$Nb_max
  Nb_increment <- args$Nb_increment
  confidence_level <- args$confidence_level
  freqs_tibble <- read_table(
    args$file,
    col_names = c("donor_freqs", "recip_freqs"),
    col_types = c(col_number(), col_number())
  )
  
  # Filter out iSNVs that are not truly in the donor -----------------------------
  original_row_count <- nrow(freqs_tibble)
  freqs_tibble <- freqs_tibble %>%
    subset(donor_freqs >= var_calling_threshold) %>%
    subset(donor_freqs <= (1 - var_calling_threshold))
  new_row_count <- nrow(freqs_tibble)
  n_variants <- new_row_count
  warnings <- warningMessage(
    opt = 1,
    suppressMessage = TRUE,
    original_row_count = original_row_count,
    new_row_count = new_row_count,
    warnings = warnings
  )
  
  ### Save number of variants going into calculation ###
  num_vars[p, 2] <- n_variants
  
  # Implement the beta binomial algorithm ----------------------------------------
  bottleneck_values_vector <- c()
  for (i in Nb_min:Nb_max) {
    if (i %% Nb_increment == 0) {
      bottleneck_values_vector <- c(bottleneck_values_vector, i)
    }
  }
  
  LL_tibble <- tibble(
    bottleneck_size = bottleneck_values_vector,
    Log_Likelihood = 0 * bottleneck_values_vector
  )
  
  for (I in 1:nrow(LL_tibble)) {
    LL_tibble$Log_Likelihood[I] <- LL_func_approx(Nb_size = LL_tibble$bottleneck_size[I])
  }
  
  # Find maximum likelihood estimate and associated confidence interval --------
  Max_LL <- max(LL_tibble$Log_Likelihood)
  Max_LL_bottleneck_index <- which(LL_tibble$Log_Likelihood == max(LL_tibble$Log_Likelihood))
  Max_LL_bottleneck <- bottleneck_values_vector[Max_LL_bottleneck_index]
  likelihood_ratio <- qchisq(confidence_level, df = 1)
  
  ci_tibble <- filter(LL_tibble, 2 * (Max_LL - Log_Likelihood) <= likelihood_ratio)
  ci_tibble <- LL_tibble %>%
    filter(2 * (Max_LL - Log_Likelihood) < likelihood_ratio)
  lower_CI_bottleneck <- min(ci_tibble$bottleneck_size)
  upper_CI_bottleneck <- max(ci_tibble$bottleneck_size)
  
  ### Adjust for fringe cases of interest ###
  corrected <- 0
  
  if (length(ci_tibble$Log_Likelihood) == 0) {
    lower_CI_bottleneck <- min(Max_LL_bottleneck)
    upper_CI_bottleneck <- max(Max_LL_bottleneck)
    corrected <- 1
  }
  if (max(Max_LL_bottleneck) == max(bottleneck_values_vector)) {
    upper_CI_bottleneck <- max(bottleneck_values_vector)
    corrected <- 2
  } else if (min(Max_LL_bottleneck) == min(bottleneck_values_vector)) {
    lower_CI_bottleneck <- min(bottleneck_values_vector)
    corrected <- 3
  }
  warnings <- warningMessage(
    opt = 2,
    suppressMessage = TRUE,
    Max_LL_bottleneck = Max_LL_bottleneck,
    bottleneck_values_vector = bottleneck_values_vector,
    warnings = warnings
  )
  
  ### Save this MLE and confidence interval information ###
  ll_out[[p]] <- LL_tibble %>% 
    mutate(pair = p)
  
  if (n_variants != 0) {
    confidence[p, 2] <- Max_LL_bottleneck
    confidence[p, 3] <- lower_CI_bottleneck
    confidence[p, 4] <- upper_CI_bottleneck
    confidence[p, 5] <- corrected
  } else {
    confidence[p, 2] <- 999
    confidence[p, 3] <- 999
    confidence[p, 4] <- 999
    confidence[p, 5] <- 999
  }
}

rm(list = setdiff(ls(), c("confidence", "ll_out", "num_vars", "warnings")))

# Export everything to files ---------------------------------------------------

write.table(num_vars,
            file = "./04 - Output/num_vars.txt",
            sep = "\t",
            row.names = FALSE)
write.table(confidence,
            file = "./04 - Output/confidence_int.txt",
            sep = "\t",
            row.names = FALSE)
write.table(warnings,
            file = "./04 - Output/warnings.txt",
            sep = "\t",
            row.names = FALSE)
write.table(ll_out %>% bind_rows(),
            file = "./04 - Output/logLikelihood.txt",
            sep = "\t",
            row.names = FALSE)
