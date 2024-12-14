## Title: Phylogenetic Trees
## Author: Katy Krupinsky
## Date Last Modified: 02/13/23
## Description: This script generates the phylogenetic trees previously
## generated from the whole-genome sequences of the study isolates for
## specifically the samples included in our final sample set.
##
## -----------------------------------------------------------------------------

## Load packages, raw data, and source script ----------------------------------

library(tidyverse)
library(readtext)
library(ape)
library(ggtree)

source("./scripts/00-samplingScheme_v2.R")
rm(list=setdiff(ls(), c("output_dataset")))

final_id_strain <- output_dataset %>% 
    select(full_id, year, reference) %>% 
    distinct()

Mich_17 <- read.tree("./treefiles/Michigan_17_tree")
Mich_18 <- read.tree("./treefiles/Michigan_18_tree")
Singapore <- read.tree("./treefiles/singapore_tree")

pairMeta <- read.table("./input/pair_meta.txt", header = TRUE) %>% 
    rename(pair = pair_id)

colors <- c("#4E79A7", "#F28E2B", "#A0CBE8", "#A0CBE8", "#FFBE7D",
                     "#59A14F", "#8CD17D", "#B6992D", "#F1CE63", "#499894",
                     "#499894", "#86BCB6", "#E15759", "#FF9D9A", "#79706E",
                     "#BAB0AC", "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8",
                     "#9D7660")

pairData <- pairMeta %>% 
    distinct() %>% 
    mutate(pair_alpha = LETTERS[1:21]) %>%
    rename(pair_id = pair) %>% 
    mutate(year = ifelse(is.na(year), 18, year)) %>% 
    rename(pair = pair_id) %>% 
    bind_cols(colors = colors)

rm(pairMeta)

## Pull out tips from the trees in preparation for later pruning ---------------

Mich_17_tips <- final_id_strain %>% 
    filter(reference == "Michigan" & year == 17) %>% 
    select(full_id) %>% 
    mutate(full_id = as.character(full_id)) %>% 
    as_vector()
Mich_17_tips <- c(Mich_17_tips, "Michigan_reference")

Mich_18_tips <- final_id_strain %>% 
    filter(reference == "Michigan" & year == 18) %>% 
    select(full_id) %>% 
    mutate(full_id = as.character(full_id)) %>% 
    as_vector()
Mich_18_tips <- c(Mich_18_tips, "Michigan_reference")

Singapore_tips <- final_id_strain %>% 
    filter(reference == "Singapore") %>% 
    select(full_id) %>% 
    mutate(full_id = as.character(full_id)) %>% 
    as_vector()
Singapore_tips <- c(Singapore_tips, "Singapore_16_concat_reference")

## Prune the trees to only include tips within our finals sample set -----------

Mich_17_pruned <- keep.tip(Mich_17, Mich_17_tips)
Mich_18_pruned <- keep.tip(Mich_18, Mich_18_tips)
Singapore_pruned <- keep.tip(Singapore, Singapore_tips)

## Create data frame which allows for consistent naming of the reference -------

Michigan_reference_17 <- data.frame(tip.label = "Michigan_reference",
                                 year = 17,
                                 subj_id = "Reference",
                                 household = NA)

Michigan_reference_18 <- data.frame(tip.label = "Michigan_reference",
                                    year = 18,
                                    subj_id = "Reference",
                                    household = NA)

Singapore_reference_18 <- data.frame(tip.label =
                                         "Singapore_16_concat_reference",
                                    year = 18,
                                    subj_id = "Reference",
                                    household = NA)

# Continued prep of the data for tree plotting ---------------------------------

Mich_17_data <- output_dataset %>%
    filter(reference == "Michigan" & year == 17) %>% 
    select(full_id, year, subj_id, household) %>% 
    distinct() %>% 
    rename(tip.label = full_id) %>% 
    mutate(tip.label = as.character(tip.label),
           subj_id = as.character(subj_id)) %>% 
    bind_rows(Michigan_reference_17) %>% 
    inner_join(pairData) %>% 
    mutate(colors = ifelse(is.na(colors), "black", colors))

Mich_18_data <- output_dataset %>% 
    filter(reference == "Michigan" & year == 18) %>% 
    select(full_id, year, subj_id, household) %>% 
    distinct() %>% 
    rename(tip.label = full_id) %>% 
    mutate(tip.label = as.character(tip.label),
           subj_id = as.character(subj_id)) %>% 
    bind_rows(Michigan_reference_18) %>% 
    inner_join(pairData) %>% 
    mutate(colors = ifelse(is.na(colors), "black", colors))

Singapore_data <- output_dataset %>% 
    filter(reference == "Singapore") %>% 
    select(full_id, year, subj_id, household) %>% 
    distinct() %>% 
    rename(tip.label = full_id) %>% 
    mutate(tip.label = as.character(tip.label),
           subj_id = as.character(subj_id)) %>% 
    bind_rows(Singapore_reference_18) %>% 
    distinct() %>% 
    inner_join(pairData) %>% 
    mutate(colors = ifelse(is.na(colors), "black", colors))

## Generation of the trees -----------------------------------------------------

Mich_17_tree <- ggtree(Mich_17_pruned) %<+%
    Mich_17_data +
    geom_tiplab(aes(label = subj_id, color = as.factor(household)),
                fontface = 2) +
    geom_tippoint() +
    geom_treescale(x = 0, y = 0, offset = 0.1, width = 0.001) +
    scale_color_manual(values = c("#4E79A7", "#F28E2B"),
                            na.value = "black",
                            name = "Household\nNumber") +
    ggtitle("Reference = 'Michigan' & Year = 2017") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -2,
                                    size = 18, face = "bold"),
          legend.position = "none") +
    hexpand(0.05)

Mich_18_tree_colors <- Mich_18_data$colors

Mich_18_tree <- ggtree(Mich_18_pruned) %<+%
    Mich_18_data +
    geom_tiplab(aes(label = subj_id, color = as.factor(household)),
                fontface = 2) +
    geom_tippoint() +
    geom_treescale(x = 0, y = 0, offset = 0.1, width = 0.001) +
    scale_color_manual(values = Mich_18_tree_colors,
                            na.value = "black",
                            name = "Household\nNumber") +
    ggtitle("Reference = 'Michigan' & Year = 2018") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -2,
                                    size = 18, face = "bold"),
          legend.position = "none") +
    hexpand(0.05)

Singapore_tree_colors <- Singapore_data %>% 
    select(colors) %>% 
    as_vector()

Singapore_tree <- ggtree(Singapore_pruned) %<+%
    Singapore_data +
    geom_tiplab(aes(label = subj_id, color = as.factor(household)),
                fontface = 2) +
    geom_tippoint() +
    geom_treescale(x = 0, y = 30, offset = 0.8, width = 0.001) +
    scale_color_manual(values = Singapore_tree_colors,
                            na.value = "black",
                            name = "Household\nNumber") +
    ggtitle("Reference = 'Singapore' & Year = 2018") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -2,
                                    size = 16, face = "bold"),
          legend.position = "none") +
    hexpand(0.05)
Singapore_tree <- flip(Singapore_tree, 52, 8)

## Generation of the final relevant figure -------------------------------------

top_row <- cowplot::plot_grid(Mich_17_tree, Mich_18_tree)
total_tree <- cowplot::plot_grid(top_row, Singapore_tree,
                   nrow = 2)  +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
total_tree
