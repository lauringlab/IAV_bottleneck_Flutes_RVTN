## -----------------------------------------------------------------------------
## Title: Supplemental Figure 2 - Onset relative to index
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(tidyverse)
library(lubridate)
source("./01 - Functions/plotDaysPostSymptomOnset.R")
source("./01 - Functions/plotDaysOnsetRelativeToIndex.R")
df <- read.table("./04 - Output/iSNV_data/pair_meta.txt", header = TRUE)

save <- TRUE
# 1. Create plot ---------------------------------------------------------------
p <- plotDaysOnsetRelativeToIndex(df)

if (save) {
  ggsave(
    plot = p,
    filename = "./05 - Figures and Tables/Rendered/suppfig2.png",
    width = 10,
    height = 12
  )
}

rm(list = setdiff(ls(), c()))