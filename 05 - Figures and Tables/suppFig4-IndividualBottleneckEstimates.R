# Name: Figure 4 - Individual Pair Bottlenecks
# ------------------------------------------------------------------------------

### Bring in functions ###
file.sources <- list.files(
  c("./01 - Functions"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

file.sources <- c(file.sources, "./01 - Functions/theme_bottleneck.R")
sapply(file.sources, source, .GlobalEnv)
rm(file.sources)

df <- setupOverallBottleneckCalculation(pathToPairMeta = "03 - Input/pair_meta.txt",
                                              pathToPairSnv = "03 - Input/pairsnv.txt",
                                              pathToConfidenceInt = "04 - Output/confidence_int.txt",
                                              pathToLogLikelihood = "04 - Output/logLikelihood.txt",
                                              pathToNumVars = "04 - Output/num_vars.txt") 
bottleneck_meta2 <- setupIndividualBottleneckPlotting(pathToPairMeta = "03 - Input/pair_meta.txt",
                                              pathToPairSnv = "03 - Input/pairsnv.txt",
                                              pathToConfidenceInt = "04 - Output/confidence_int.txt",
                                              pathToLogLikelihood = "04 - Output/logLikelihood.txt",
                                              pathToNumVars = "04 - Output/num_vars.txt")

### Individual Transmisison Pair Bottleneck Calculation ###
bottleneck_matrix <- calculateOverallBottleneck(df)

### Generate plots ###
a <- plotBottleneckWithNumberOfVariants(
  bottleneck_meta = bottleneck_meta2,
  bottleneck_matrix = bottleneck_matrix
)

# ggsave(
#   plot = a,
#   filename = "./05 - Figures/renderedPlots/f4a.png",
#   width = 13,
#   height = 8
# )
