## -----------------------------------------------------------------------------
## Title: Supplemental Figure 4 - Onset relative to index
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and functions --------------------------------------------
source("~/git/FluTES_bottleneck/01 - Functions/setupOverallBottleneckCalculation.R")
source("~/git/FluTES_bottleneck/01 - Functions/setupIndividualBottleneckPlotting.R")
source("~/git/FluTES_bottleneck/01 - Functions/calculateOverallBottleneck.R")
source("~/git/FluTES_bottleneck/01 - Functions/plotBottleneckWithNumberOfVariants.R")

df <- setupOverallBottleneckCalculation(pathToPairMeta =  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt",
                                              pathToPairSnv = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pairsnv.txt",
                                              pathToConfidenceInt = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/confidence_int.txt',
                                              pathToLogLikelihood = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/logLikelihood.txt',
                                              pathToNumVars = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/num_vars.txt') 
bottleneck_meta2 <- setupIndividualBottleneckPlotting(pathToPairMeta =  "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pair_meta.txt",
                                                      pathToPairSnv = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pairsnv.txt",
                                                      pathToConfidenceInt = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/confidence_int.txt',
                                                      pathToLogLikelihood = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/logLikelihood.txt',
                                                      pathToNumVars = '/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/01 - Output/num_vars.txt') 

### Individual Transmisison Pair Bottleneck Calculation ###
bottleneck_matrix <- calculateOverallBottleneck(df)

### Generate plots ###
p <- plotBottleneckWithNumberOfVariants(
  bottleneck_meta = bottleneck_meta2,
  bottleneck_matrix = bottleneck_matrix
)

ggsave(plot = p,
       filename = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/01 - figures/suppfig4.png",
       width = 13,
       height = 8,
       bg = "white")
