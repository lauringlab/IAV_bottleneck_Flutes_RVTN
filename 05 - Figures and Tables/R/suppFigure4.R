## -----------------------------------------------------------------------------
## Title: Supplemental Figure 4 - Onset relative to index
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and functions --------------------------------------------
source("./01 - Functions/setupOverallBottleneckCalculation.R")
source("./01 - Functions/setupIndividualBottleneckPlotting.R")
source("./01 - Functions/calculateOverallBottleneck.R")
source("./01 - Functions/plotBottleneckWithNumberOfVariants.R")

df <- setupOverallBottleneckCalculation(
  pathToPairMeta =  "./04 - Output/iSNV_data/pair_meta.txt",
  pathToPairSnv = "./04 - Output/iSNV_data/pairsnv.txt",
  pathToConfidenceInt = './04 - Output/iSNV_data/01 - Output/confidence_int.txt',
  pathToLogLikelihood = './04 - Output/iSNV_data/01 - Output/logLikelihood.txt',
  pathToNumVars = './04 - Output/iSNV_data/01 - Output/num_vars.txt'
) %>%
  dplyr::filter(n_variants != 0)

bottleneck_meta2 <- setupIndividualBottleneckPlotting(
  pathToPairMeta =  "./04 - Output/iSNV_data/pair_meta.txt",
  pathToPairSnv = "./04 - Output/iSNV_data/pairsnv.txt",
  pathToConfidenceInt = './04 - Output/iSNV_data/01 - Output/confidence_int.txt',
  pathToLogLikelihood = './04 - Output/iSNV_data/01 - Output/logLikelihood.txt',
  pathToNumVars = './04 - Output/iSNV_data/01 - Output/num_vars.txt'
)

### Individual Transmisison Pair Bottleneck Calculation ###
bottleneck_matrix <- calculateOverallBottleneck(df)

### Generate plots ###
p <- plotBottleneckWithNumberOfVariants(bottleneck_meta = bottleneck_meta2, bottleneck_matrix = bottleneck_matrix)

save <- TRUE

if (save) {
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/tif/suppfig4.tif",
    width = 13,
    height = 8,
    bg = "white"
  )
  
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/eps/suppfig4.eps",
    width = 13,
    height = 8,
    bg = "white"
  )
  
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/png/suppfig4.png",
    width = 13,
    height = 8,
    bg = "white"
  )
}

rm(list = setdiff(ls(), c()))