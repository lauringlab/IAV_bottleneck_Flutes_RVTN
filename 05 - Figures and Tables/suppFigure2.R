## -----------------------------------------------------------------------------
## Title: Supplemental Figure 2 - Onset relative to index
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(tidyverse)
library(lubridate)
source("~/git/FluTES_bottleneck/01 - Functions/plotDaysPostSymptomOnset.R")
source("~/git/FluTES_bottleneck/01 - Functions/plotDaysOnsetRelativeToIndex.R")
df <- read.table("./04 - Output/iSNV_data/pair_meta.txt", header = TRUE)

save <- FALSE
# 1. Create plot ---------------------------------------------------------------
p <- plotDaysOnsetRelativeToIndex(df)

if (save) {
  ggsave(
    plot = p,
    filename = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/01 - figures/suppfig3.png",
    width = 10,
    height = 12
  )
}
