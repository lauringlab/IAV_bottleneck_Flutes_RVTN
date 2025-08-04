## -----------------------------------------------------------------------------
## Title: Figure 3 - Bottleneck estimates for both methods
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(tidyverse)

source("~/git/FluTES_bottleneck/01 - Functions/calculateBottleneckByMetadata.R")
source("~/git/FluTES_bottleneck/01 - Functions/calculateBottleneckByLevels.R")
source("~/git/FluTES_bottleneck/01 - Functions/calculateOverallBottleneck.R")
source("~/git/FluTES_bottleneck/01 - Functions/formatClonalDataForPlotting.R")
source("~/git/FluTES_bottleneck/01 - Functions/makePairTable.R")
source(
  "~/git/FluTES_bottleneck/01 - Functions/plotBottleneckSizeByMetadataBars_bothMethods.R"
)
source("~/git/FluTES_bottleneck/01 - Functions/findCI.R")

# 1. Format data ---------------------------------------------------------------
isnv_pair <- read.table(
  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt",
  header = TRUE
) %>%
  # Remove the pairs that were removed from the dataset due to no iSNVs
  filter(pair_id != 55 & pair_id != 62 & pair_id != 63)

clonal_pair <- read.table(
  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_dist_with_meta.txt",
  header = TRUE
)

isnvData <- calculateBottleneckByMetadata(
  pathToConfidenceInterval = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/confidence_int.txt',
  pathToLogLikelihood = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/logLikelihood.txt',
  pathToNumVars = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/num_vars.txt',
  pathToPairMeta = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt"
)

clonalData <- formatClonalDataForPlotting(pathToOutput = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_mut_all_threshhold_trimmed.csv",
                                          isnv_pair = isnv_pair,
                                          clonal_pair = clonal_pair) %>%
  na.omit()


# 2. Create plot ---------------------------------------------------------------
p <- plotBottleneckSizeByMetadataBars_bothMethods(isnvData = isnvData, clonalData = clonalData)

ggsave(
  filename = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/01 - figures/fig3.png',
  plot = p,
  width = 15,
  height = 8
)
