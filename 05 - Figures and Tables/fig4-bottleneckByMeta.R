# Name: Figure 4 - Bottleneck size by co-factors
# ------------------------------------------------------------------------------

### Bring in functions ###
file.sources <- list.files(
  c("./01 - Functions"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

sapply(file.sources, source, .GlobalEnv)
rm(file.sources)

bottleneck_by_metadata <- calculateBottleneckByMetadata(
  pathToConfidenceInterval = "04 - Output/confidence_int.txt",
  pathToLogLikelihood = "04 - Output/logLikelihood.txt",
  pathToNumVars = "04 - Output/num_vars.txt",
  pathToPairMeta = "03 - Input/pair_meta.txt"
)

### Make the plot ###
a <- plotBottleneckSizeByMetadataBars(bottleneck_by_metadata, opt = "all")

# ggsave(
#   plot = a,
#   filename = "./05 - Figures/renderedPlots/f6a.png",
#   width = 15,
#   height = 8
# )
