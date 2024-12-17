#Figure 1 - Descriptive data
file.sources <- list.files(
  c("./01 - Functions"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

sapply(file.sources, source, .GlobalEnv)
rm(file.sources)


a <- plotDelayCollection(pathToPairMeta = './03 - Input/pair_meta.txt')
b <- plotSerialIntervalOverall(pathToPairMeta = './03 - Input/pair_meta.txt')
c <- plotSerialSubfactors(pathToPairMeta = './03 - Input/pair_meta.txt')
