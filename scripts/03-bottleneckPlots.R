## Title: Bottleneck Plots
## Author: Katy Krupinsky
## Date Last Modified: 02/13/23
## Description: This script takes the output from the BottleneckCalculation.R
## script and creates data visualizations.
##
## -----------------------------------------------------------------------------

## Path names - to be updated depending on local location of data --------------

path_to_raw_data <- "./imput/IAV_meta_snv.csv"
path_to_pair_meta <- "./input/pair_meta/txt"
path_to_varient_counts <- "./bottleneckOutput/all_zero.txt"
path_to_confidence_int <- "./bottleneckOutput/confidence_int.txt"

## Load packages and raw data --------------------------------------------------
library(tidyverse)
library(ggthemes)
library(paletteer)
library(ggnewscale)
library(ggforce)
library(ggpattern)

pairMeta <- read.table(path_to_pair_meta, header = TRUE) %>% 
    rename(pair = pair_id)
varientCounts <- read.table(path_to_varient_counts, header = TRUE) 

pairData <- full_join(pairMeta, varientCounts) %>% 
    distinct() %>% 
    mutate(pair_alpha = LETTERS[1:21]) %>%
    rename(pair_id = pair) %>% 
    mutate(year = ifelse(is.na(year), 18, year)) %>% 
    rename(pair = pair_id)

rm(pairMeta)
rm(varientCounts)

## Create plot of number of variants used within bottleneck calculation --------

pairInformation <- pairData %>% 
    ggplot() +
    geom_col(aes(x = as.factor(pair_alpha), y = n_variants,
                 fill = as.factor(household))) +
    theme_hc(base_size = 18) +
    scale_fill_paletteer_d("ggthemes::Tableau_20",
                           name = "Household\nNumber") +
    new_scale_color() +
    geom_rug(aes(x = as.factor(pair_alpha), color = as.factor(year)),
             linewidth = 23) +
    scale_color_paletteer_d("ggsci::alternating_igv",
                            labels = c("2017-2018", "2018-2019"),
                            name = "Collection\nSeason") +
    xlab("Pair Number") +
    ylab("Number of Varients Used in Bottleneck Calculation") +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

## Import output from bottleneckOutput.R script --------------------------------

pairs <- read.table(path_to_varient_counts) %>% 
    filter(V2 == FALSE) %>% 
    select(V1) %>% 
    mutate(V1 = as.character(V1)) %>% 
    as_vector()

list <- vector("list")

for (i in pairs){
    filename <- paste("./bottleneckOutput/",
                      paste(paste("output_pair", i, sep = "_"), ".txt",
                            sep = ""),
                      sep = "")
    objectname <- paste("pair_", i, sep = "")
    df <- read.table(filename, sep = "\t") %>% 
        mutate(pair = as.character(i))
    list[[i]] <- df
}

confidence <- read.table(path_to_confidence_int) %>% 
    purrr::set_names(as.character(slice(., 1))) %>%
    slice(-1) %>% 
    na.omit() %>% 
    mutate(pair = as.double(pair))

## Plot MLE for each individual transmission pair ------------------------------

df <- bind_rows(list) %>% 
    rename(bottleneck_size = V1,
           log_liklihood = V2) %>% 
    mutate(pair = as.numeric(pair)) %>% 
    full_join(confidence) %>% 
    mutate(max_LL = as.numeric(max_LL),
           upper_CI = as.numeric(upper_CI),
           lower_CI = as.numeric(lower_CI)) %>% 
    inner_join(pairData)

# pairBottlenecks <- df %>% 
#     ggplot() +
#     geom_line(aes(x = bottleneck_size, y = log_liklihood,
#                   linetype = as.factor(year))) +
#     geom_vline(mapping = aes(xintercept = max_LL), col = "red") +
#     geom_vline(aes(xintercept = lower_CI), col = "blue", lty = 2) +
#     geom_vline(aes(xintercept = upper_CI), col = "blue", lty = 2) +
#     facet_wrap(~pair, scales = "free") +
#     theme_hc(base_size = 16) +
#     labs(linetype = "Year") +
#     xlab("Bottleneck Size") +
#     ylab("Log-Likelihood")

confidence_mean_total <- confidence %>% 
    mutate(max_LL = as.numeric(max_LL)) %>% 
    summarize(mean(max_LL)) %>% 
    as_vector()

confidencePlot <- confidence %>% 
    inner_join(pairData) %>% 
    mutate(lower_CI = as.numeric(lower_CI),
           upper_CI = as.numeric(upper_CI),
           max_LL = as.numeric(max_LL)) %>% 
    ggplot() +
    geom_bar(aes(x = as.factor(pair_alpha), y = max_LL,
                 fill = as.factor(household)),
             stat = "identity") +
    scale_fill_paletteer_d("ggthemes::Tableau_20",
                           name = "Household\nNumber") +
    new_scale_color() +
    geom_rug(aes(x = as.factor(pair_alpha), color = as.factor(year)),
             linewidth = 15) +
    scale_color_paletteer_d("ggsci::alternating_igv",
                            labels = c("2017-2018", "2018-2019"),
                            name = "Collection\nSeason") +
    geom_errorbar(aes(x = as.factor(pair_alpha),
                      ymin = lower_CI, ymax = upper_CI)) +
    geom_hline(yintercept = confidence_mean_total, lty = 2) +
    theme_hc(base_size = 14) +
    xlab("Transmission Pair") +
    ylab("Bottleneck Size") +
    labs(color = "Number of Varients") +
    ggforce::facet_zoom(ylim = c(0, 15)) +
    scale_y_continuous(breaks = pretty) +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

## Calculate the overall bottleneck size ---------------------------------------

max_num_snps <- max(df$n_variants)

total_LL <- df %>% 
    select(bottleneck_size, log_liklihood, n_variants) %>% 
    group_by(bottleneck_size) %>% 
    mutate(adjusted_LL = (max_num_snps / n_variants) * log_liklihood) %>% 
    summarise(adjusted_LL = sum(adjusted_LL))

Max_LL <- max(total_LL$adjusted_LL) 
Max_LL_bottleneck_index <- 
    which(total_LL$adjusted_LL == max(total_LL$adjusted_LL)) 
Max_LL_bottleneck <- total_LL$bottleneck_size[Max_LL_bottleneck_index] 
likelihood_ratio <- qchisq(0.95, df = 1) 
ci_tibble <- filter(total_LL,
                    2 * (Max_LL - adjusted_LL) <= likelihood_ratio) 
lower_CI_bottleneck <- min(ci_tibble$bottleneck_size) 
upper_CI_bottleneck <- max(ci_tibble$bottleneck_size) 

total_confidence <- data.frame(max_LL = Max_LL_bottleneck,
                              lower_CI = lower_CI_bottleneck,
                              upper_CI = upper_CI_bottleneck)

## Plot overall bottleneck size ------------------------------------------------

total_LL_plot <- total_LL %>% 
    filter(adjusted_LL != -Inf) %>% 
    bind_rows(total_confidence) %>% 
    ggplot() +
    geom_point(aes(x = bottleneck_size, y = adjusted_LL), col = "#5C6068") +
    geom_vline(mapping = aes(xintercept = max_LL), col = "#E15759",
               lwd = 1) +
    geom_vline(aes(xintercept = lower_CI), col = "#4E79A7", lty = 2,
               lwd = 1) +
    geom_vline(aes(xintercept = upper_CI), col = "#4E79A7", lty = 2,
               lwd = 1) +
    theme_classic(base_size = 18) +
    xlab("Bottleneck Size") +
    ylab("Log-Likelihood") +
    annotate("text", x = 3, y = -32000,
             label = paste("MLE = ", Max_LL_bottleneck, sep = ""),
             size = 6, col = "#E15759", hjust = 0) +
    annotate("text", x = 3, y = -34000,
             label = paste(paste(paste(paste("95% CI = (",
                                       lower_CI_bottleneck, sep = ""),
                                       ", ", sep = ""),
                           upper_CI_bottleneck, sep = ""), ")", sep = ""), 
             size = 6, col = "#4E79A7", hjust = 0) +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
