## -----------------------------------------------------------------------------
## Title: Figure 2 - Pair iSNV and Clonal Distribution Characteristics
## Author: Katy Krupinsky
## Last Updated: 04/09/25
## -----------------------------------------------------------------------------

# 0. Load libraries and import data --------------------------------------------
library(tidyverse)
library(patchwork)
source("~/git/FluTES_bottleneck/01 - Functions/plotTvPlot.R")
source("~/git/FluTES_bottleneck/01 - Functions/plotClonalDistribution.R")

pair_tv <- read.table("/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/iSNV_data/pairsnv_tv.txt",
                      header = TRUE)
pair_clonal <- read.table("/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/Clonal_data/clonal_dist_with_meta.txt",
                          header = TRUE)

# 1. Create plot ---------------------------------------------------------------
a <- plotTvPlot(pair_tv, var_thresh = 0.05, out = 1)
b <- plotClonalDistribution(pair_clonal)

fig2 <- a + b + plot_annotation(tag_levels = "A")

ggsave(plot = fig2,
       filename = "/Users/katykrupinsky/Documents/College/03-UM/Research/Papers/Bottlenecks/01 - figures/fig2.png",
       width = 13,
       height = 5)
