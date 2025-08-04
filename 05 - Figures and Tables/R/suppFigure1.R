## -----------------------------------------------------------------------------
## Title: Supplemental Figure 1 - Phylogenetic trees
## Author: Katy Krupinsky
## Last Updated: 04/24/25
## -----------------------------------------------------------------------------

# 0. Load libraries, functions, and pathnames ----------------------------------
library(ggtree)
library(tidyverse)
library(PNWColors)
library(paletteer)
library(patchwork)

source("~/git/FluTES_bottleneck/01 - Functions/makeTree.R")

pathToMichigan2017Tree <- './03 - Input/treeData/01 - michigan_2017/aligned_seqs.fasta.treefile'
pathToSingapore2018Tree <- './03 - Input/treeData/02 - singapore_2018/alignedSequences2.fasta.treefile'
pathToBrisbane2019Tree <- './03 - Input/treeData/03 - brisbane_2019/aligned_seqs.fasta.treefile'
pathToDarwin2021Tree <- './03 - Input/treeData/04 - darwin_2021/Darwin_H3N2_2021_alignedFile.fasta.treefile'
pathToMeta <- "./04 - Output/iSNV_data/sample_data_with_meta.txt"
pathToPair <- "./04 - Output/iSNV_data/pair_meta.txt"

save <- TRUE

# 1. Create plots --------------------------------------------------------------
a <- makeTree(
  pathToMeta = pathToMeta,
  strain_int = "Mighigan_H1N1_2017",
  pathToTreeFile = pathToMichigan2017Tree,
  pathToPair = pathToPair,
  scale_pos = 5.3
)

b <- makeTree(
  pathToMeta = pathToMeta,
  strain_int = "Singapore_H3N2_2018",
  pathToTreeFile = pathToSingapore2018Tree,
  pathToPair = pathToPair,
  scale_pos = 35
)

c <- makeTree(
  pathToMeta = pathToMeta,
  strain_int = "Brisbane_H1N1_2019",
  pathToTreeFile = pathToBrisbane2019Tree,
  pathToPair = pathToPair,
  scale_pos = 25
)

d <- makeTree(
  pathToMeta = pathToMeta,
  strain_int = "Darwin_H3N2_2021",
  pathToTreeFile = pathToDarwin2021Tree,
  pathToPair = pathToPair,
  scale_pos = 35
)

if (save) {
  ggsave(
    plot = a,
    filename = "./05 - Figures and Tables/Rendered/suppfig1/suppfig1_a.png",
    width = 9,
    height = 2,
    bg = "white"
  )
  
  ggsave(
    plot = b,
    filename = "./05 - Figures and Tables/Rendered/suppfig1/suppfig1_b.png",
    width = 10,
    height = 8,
    bg = "white"
  )
  
  ggsave(
    plot = c,
    filename = "./05 - Figures and Tables/Rendered/suppfig1/suppfig1_c.png",
    width = 9,
    height = 6,
    bg = "white"
  )
  
  ggsave(
    plot = d,
    filename = "./05 - Figures and Tables/Rendered/suppfig1/suppfig1_d.png",
    width = 9,
    height = 13,
    bg = "white"
  )
}

rm(list = setdiff(ls(), c()))

### Paneled together using illustrator ###
