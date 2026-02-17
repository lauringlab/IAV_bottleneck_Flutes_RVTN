## -----------------------------------------------------------------------------
## Title: Figure 3 - Bottleneck estimates for both methods
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(tidyverse)

source("./01 - Functions/calculateBottleneckByMetadata.R")
source("./01 - Functions/calculateBottleneckByLevels.R")
source("./01 - Functions/calculateOverallBottleneck.R")
source("./01 - Functions/formatClonalDataForPlotting.R")
source("./01 - Functions/makePairTable.R")
source("./01 - Functions/plotBottleneckSizeByMetadataBars_bothMethods.R")
source("./01 - Functions/findCI.R")

noFive <- FALSE
if (noFive) {
  pathToClonalOutput <- "./04 - Output/Clonal_data/clonal_mut_all_output_noFive.csv"
} else {
  pathToClonalOutput <- "./04 - Output/Clonal_data/clonal_mut_all_output.csv"
}

# 1. Format data ---------------------------------------------------------------
nonzero_pairs <- read.table("./04 - Output/iSNV_data/01 - Output/num_vars.txt", header = TRUE) %>%
  dplyr::filter(n_variants != 0) %>%
  dplyr::select(pair) %>%
  as_vector()

isnv_pair <- read.table("./04 - Output/iSNV_data/pair_meta.txt", header = TRUE) %>%
  dplyr::filter(pair_id %in% nonzero_pairs)

clonal_pair <- read.table("./04 - Output/Clonal_data/clonal_dist_with_meta.txt",
                          header = TRUE)

if (noFive == TRUE) {
  isnv_pair <- isnv_pair %>% filter(donor_id != 1809801502 &
                                      recipient_id != 1809802501)
  clonal_pair <- clonal_pair %>% filter(donor_id != 1809801502 &
                                          recipient_id != 1809802501)
}

isnvData <- calculateBottleneckByMetadata(
  pathToConfidenceInterval = './04 - Output/iSNV_data/01 - Output/confidence_int.txt',
  pathToLogLikelihood = './04 - Output/iSNV_data/01 - Output/logLikelihood.txt',
  pathToNumVars = './04 - Output/iSNV_data/01 - Output/num_vars.txt',
  pathToPairMeta = "./04 - Output//iSNV_data/pair_meta.txt",
  noFive = noFive
)

clonalData <- formatClonalDataForPlotting(pathToOutput = pathToClonalOutput,
                                          isnv_pair = isnv_pair,
                                          clonal_pair = clonal_pair) %>%
  na.omit()


# 2. Create plot ---------------------------------------------------------------
p <- plotBottleneckSizeByMetadataBars_bothMethods(isnvData = isnvData, clonalData = clonalData)

save <- TRUE

if (save) {
  if (noFive) {
    ggsave(
      filename = './05 - Figures and Tables/Rendered/png/fig3_noFive.png',
      plot = p,
      width = 15,
      height = 8
    )
  } else {
    ggsave(
      filename = './05 - Figures and Tables/Rendered/tif/fig3.tif',
      plot = p,
      width = 15,
      height = 8
    )
    
    ggsave(
      filename = './05 - Figures and Tables/Rendered/eps/fig3.eps',
      plot = p,
      width = 15,
      height = 8
    )
    
    ggsave(
      filename = './05 - Figures and Tables/Rendered/png/fig3.png',
      plot = p,
      width = 15,
      height = 8
    )
    
  }
}

rm(list = setdiff(ls(), c()))
