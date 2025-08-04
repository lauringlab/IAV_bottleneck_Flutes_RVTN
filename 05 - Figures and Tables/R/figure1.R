## -----------------------------------------------------------------------------
## Title: Figure 1 - Sample iSNV Characteristics
## Author: Katy Krupinsky
## Last Updated: 04/07/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(PNWColors)
library(ggpmisc)
library(patchwork)
library(dplyr)

df <- read.table("./04 - Output/iSNV_data/sample_data_with_meta.txt", 
                 header = TRUE)

source("~/git/FluTES_bottleneck/01 - Functions/plotReplicateSnv.R")
source("~/git/FluTES_bottleneck/01 - Functions/plotISnvDistribution.R")
source("~/git/FluTES_bottleneck/01 - Functions/plotMutationTypeDensity.R")

# 1. Create plot ---------------------------------------------------------------
a <- plotISnvDistribution(df)
b <- plotReplicateSnv(df)
c <- plotMutationTypeDensity(df)

fig1 <- a / (b + c) + plot_annotation(tag_levels = "A")

save <- TRUE

if(save) {
ggsave(
  plot = fig1,
  filename = "./05 - Figures and Tables/Rendered/fig1.png",
  width = 13,
  height = 8
)
}

rm(list = setdiff(ls(), c()))
