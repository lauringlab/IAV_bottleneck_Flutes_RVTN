## Title: Other Descriptive Plots
## Author: Katy Krupinsky
## Date Last Modified: 02/23/23
## Description: This script generates the iSNV frequency plots and the TV plot.
##
## -----------------------------------------------------------------------------

## Path names - to be updated depending on local location of data --------------

path_to_pair_meta <- "./input/pair_meta.txt"

path_to_dataCleaning_filtered <- "/Users/katykrupinsky/git/FluTES_bottleneck/scripts/01.a-dataCleaning_filtered.R"
path_to_dataCleaning_unfiltered <- "/Users/katykrupinsky/git/FluTES_bottleneck/scripts/01.b-dataCleaning_unfiltered.R"

## Read in libraries and import data -------------------------------------------

library(tidyverse)
library(ggthemes)
library(lemon)

pair_meta <- read.table(path_to_pair_meta)

### NOTE: Check to make sure the consensus file stuff didn't go to icloud!!! ###

source(path_to_dataCleaning_filtered)

## TV plot ---------------------------------------------------------------------

# Pull out those rows which have either column with an N/A                   ---
df_tv_nonZero <- df2 %>% 
    rename(donor_freq2 = donor_freq,
           recipient_freq2 = recipient_freq) %>%
    na.omit() %>% 
    mutate(non_fixed = 0)

# Create donor-recipient pairs without any filtering for minimum values      ---
## -----------------------------------------------------------------------------

source(path_to_dataCleaning_unfiltered)

df_tv2 <- df5 %>% 
    select(pair_id, donor, recipient, year, household, genome_segment,
           segment_position, donor_ref_allele, donor_alt_allele,
           donor_freq2, recipient_ref_allele, recipient_alt_allele,
           recipient_freq2) %>% 
    filter(!is.na(donor_freq2))

## TV plot with both arms ------------------------------------------------------
df_tv <- anti_join(df_tv2, df_tv_nonZero) %>% 
    bind_rows(df_tv_nonZero) %>% 
    full_join(pair_meta, multiple = "all") %>% 
    arrange(pair_id, donor, recipient, genome_segment, segment_position,
            donor_freq2, recipient_freq2) %>% 
    mutate(non_fixed = ifelse(is.na(non_fixed), 1, 0))

# rm(list=setdiff(ls(), c("df_tv", "pair_meta")))

tv_plot <-  df_tv %>% 
    ggplot(aes(x = donor_freq2, y = recipient_freq2)) +
    geom_point(aes(col = as.factor(non_fixed), alpha = as.factor(non_fixed)),
               size = 3, shape = 20) +
    theme_bw(base_size = 16) +
    scale_x_continuous(expand = c(0.01,0.01), ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    xlab("Frequency in Donor") +
    ylab("Frequency in Recipient") +
    scale_color_manual(values = c("red", "black"),
                       labels = c("Fixed OR\nBelow Theshold\n",
                                  "Non-Fixed AND\nAbove Threshold"),
                       name = "") +
    scale_alpha_manual(values = c(1, 0.5), guide = "none") +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
          axis.text.x = element_text(angle = 0, vjust = 0.3, hjust=0.3),
          panel.spacing = unit(2, "lines")) 

## TV plot - faceted -----------------------------------------------------------

colors <- c("#4E79A7", "#F28E2B", "#A0CBE8", "#A0CBE8", "#FFBE7D",
                     "#59A14F", "#8CD17D", "#B6992D", "#F1CE63", "#499894",
                     "#499894", "#86BCB6", "#E15759", "#FF9D9A", "#79706E",
                     "#BAB0AC", "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8",
                     "#9D7660", "#D7B5A6")

pairs_info <- pair_meta %>% 
    arrange(pair_id) %>% 
    mutate(pair_alpha = LETTERS[1:22],
           colors = colors)

pairs <- pairs_info %>% 
    select(pair_id) %>% 
    as_vector()

plot_list <- list()

for (i in pairs) {
    df_facet <- df_tv %>% 
        filter(pair_id == i) %>% 
        mutate(non_fixed = ifelse(non_fixed == 0, NA, non_fixed))
    plot_color <- pairs_info$colors[i]
    plot_title <- pairs_info$pair_alpha[i]
    plot <- df_facet %>% 
        ggplot(aes(x = donor_freq2, y = recipient_freq2)) +
        geom_point(aes(color = as.factor(non_fixed),
                       alpha = as.factor(non_fixed)),
                   size = 3, shape = 20, show.legend = FALSE) +
        theme_classic(base_size = 7) +
        geom_rect(aes(xmin = 0, xmax = 1, ymin = 1.05, ymax = 1.2),
                  fill = plot_color) +
        ylim(c(0, 1.2)) +
        scale_x_continuous(expand = c(0.01,0.01)) +
        scale_y_continuous(expand = c(0, 0.05), breaks = seq(0, 1, by = 0.25)) +
        xlab("Donor") +
        ylab("Recipient") +
        scale_color_manual(values = c("black"), na.value = "red") +
        scale_alpha_manual(values = c(0.5), na.value = 1) +
        theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
              axis.text.x = element_text(angle = 0, vjust = 0.3, hjust=0.3)) +
        coord_capped_cart(left = 'top') +
        annotate('text', x = 0.50, y = 1.13, label = plot_title,
                 size = 3, fontface = "bold", color = "white") +   
        theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))
        
    plot_list[[i]] <- plot
}

tv_plot_facet <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]],
                                    plot_list[[3]], plot_list[[4]],
                                    plot_list[[5]], plot_list[[6]],
                                    plot_list[[7]], plot_list[[8]],
                                    plot_list[[9]], plot_list[[10]],
                                    plot_list[[11]], plot_list[[12]],
                                    plot_list[[13]], plot_list[[14]],
                                    plot_list[[15]], plot_list[[16]],
                                    plot_list[[17]], plot_list[[18]],
                                    plot_list[[19]], plot_list[[20]],
                                    plot_list[[21]], plot_list[[22]],
                                    ncol = 4) +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

## iSNV/sample plot ------------------------------------------------------------

df_iSNV_sample <- df %>% 
    group_by(full_id) %>% 
    count()

iSNV_sample_mean <- mean(df_iSNV_sample$n)
iSNV_sample_median <- median(df_iSNV_sample$n)

iSNV_sample <- df_iSNV_sample %>% 
    ggplot(aes(x = n)) +
    geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
    scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 10),
                       expand = c(0.01,0.01)) +
    scale_y_continuous(breaks = pretty) +
    geom_vline(xintercept = iSNV_sample_mean, col = "purple") +
    xlab("iSNV per specimens") +
    ylab("Number of specimens") +
    theme_classic(base_size = 18) +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

## iSNV frequency plot ---------------------------------------------------------

df_frequency <- df %>% 
    select(avg_freq) %>% 
    mutate(avg_freq = as.numeric(avg_freq)) %>% 
    arrange(avg_freq)

frequency_hist <- df_frequency %>% 
    ggplot(aes(x = avg_freq)) +
    geom_histogram(binwidth = 0.025, closed = "left") +
    theme_classic(base_size = 18) +
    xlab("SNV frequency") +
    ylab("Log Number of SNV") +
    scale_x_continuous(breaks = pretty) +
    scale_y_log10() +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm")) 
