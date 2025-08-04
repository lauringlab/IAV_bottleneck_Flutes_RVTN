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
nonzero_pairs <- read.table("./04 - Output/iSNV_data/01 - Output/num_vars.txt", header = TRUE) %>%
  dplyr::filter(n_variants != 0) %>%
  dplyr::select(pair) %>%
  as_vector()

isnv_pair <- read.table("./04 - Output/iSNV_data/pair_meta.txt", header = TRUE) %>%
  dplyr::filter(pair_id %in% nonzero_pairs)

clonal_pair <- read.table("./04 - Output/Clonal_data/clonal_dist_with_meta.txt",
                          header = TRUE)

isnvData <- calculateBottleneckByMetadata(
  pathToConfidenceInterval = './04 - Output/iSNV_data/01 - Output/confidence_int.txt',
  pathToLogLikelihood = './04 - Output/iSNV_data/01 - Output/logLikelihood.txt',
  pathToNumVars = './04 - Output/iSNV_data/01 - Output/num_vars.txt',
  pathToPairMeta = "./04 - Output//iSNV_data/pair_meta.txt"
)

clonalData <- formatClonalDataForPlotting(pathToOutput = "./04 - Output/Clonal_data/clonal_mut_all_threshhold_trimmed.csv",
                                          isnv_pair = isnv_pair,
                                          clonal_pair = clonal_pair) %>%
  na.omit()


# 2. Create plot ---------------------------------------------------------------
p <- plotBottleneckSizeByMetadataBars_bothMethods(isnvData = isnvData, clonalData = clonalData)

save <- TRUE

if (save) {
  ggsave(
    filename = './05 - Figures and Tables/Rendered/fig3.png',
    plot = p,
    width = 15,
    height = 8
  )
}

rm(list = setdiff(ls(), c()))
