## Title: Other Descriptive Plots
## Author: Katy Krupinsky
## Date Last Modified: 02/13/23
## Description: This script generates the iSNV frequency plots and the TV plot.
##
## -----------------------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(unikn)
library(lemon)

### Check to make sure the consensus file stuff didn't go to icloud!!!! ###

source("./scripts/01-dataCleaning.R")

## TV plot ---------------------------------------------------------------------

# Pull out those rows which have either column with an N/A                   ---
df_tv_nonZero <- df2 %>% 
    rename(donor_freq2 = donor_freq,
           recipient_freq2 = recipient_freq) %>%
    na.omit() %>% 
    mutate(non_fixed = 1)

# Create donor-recipient pairs without any filtering for minimum values      ---
## -----------------------------------------------------------------------------

source("./scripts/00-samplingScheme_v2.R")

df <- output_dataset %>% 
    mutate(index2 = ifelse(index == 0, 2, 1))

# Do more work to get the samples arranged by onset date                     ---

donor_recipient <- df %>% 
    select(subj_id, year, household, onset_date, collection_date,
           index2, full_id) %>% 
    distinct() %>% 
    mutate(onset_date = ifelse(is.na(onset_date), collection_date,
                               onset_date)) %>% 
    select(-collection_date) %>% 
    mutate(onset_date = as.Date(onset_date, origin)) %>% 
    arrange(year, household, onset_date, index2) %>% 
    group_by(year, household) %>% 
    mutate(sample_order = row_number()) %>% 
    mutate(type = ifelse(sample_order == 1, "donor",
                         ifelse(sample_order == 2,
                                "recipient_1", "recipient_2"))) %>% 
    ungroup() %>% 
    select(year, household, type, full_id) %>% 
    spread(type, full_id) %>% 
    select(year, household, donor, recipient_1, recipient_2) %>% 
    gather(x, recipient, -c(year, household, donor)) %>% 
    na.omit() %>% 
    arrange(year, household) %>% 
    select(-x) %>% 
    mutate(pair_id = row_number())

# Pull out donor and recipient ids                                           ---
donor_ids <- donor_recipient %>% 
    select(pair_id, donor)

recipient_ids <- donor_recipient %>% 
    select(pair_id, recipient)

# Pull out exact mutations for our donor and recipient ids                   ---
donor_mutations <- donor_ids %>% 
    rename(full_id = donor) %>% 
    inner_join(df) %>% 
    select(pair_id, full_id, year, household, genome_segment, segment_position,
           ref_allele, alt_allele, avg_freq) %>% 
    rename(donor_ref_allele = ref_allele, 
           donor_alt_allele = alt_allele,
           donor_freq = avg_freq,
           donor_id = full_id)

recipient_mutations <- recipient_ids %>% 
    rename(full_id = recipient) %>% 
    inner_join(df) %>% 
    select(pair_id, full_id, year, household, genome_segment, segment_position,
           ref_allele, alt_allele, avg_freq) %>% 
    rename(recipient_ref_allele = ref_allele, 
           recipient_alt_allele = alt_allele,
           recipient_freq = avg_freq,
           recipient_id = full_id)

## Look up the consensus alleles are for each of these samples -----------------

# Get our final list of ids                                                  ---
final_ids <- df %>% 
    select(full_id) %>% 
    distinct()

# Get our sequence data into a usable format                                ---
final_sequences_wide <- inner_join(final_ids, all_sequences)
final_sequences_long <- final_sequences_wide %>% 
    gather(position, base, -c(full_id, genome_segment)) %>% 
    na.omit() %>% 
    rename(segment_position = position) %>% 
    mutate(segment_position = as.integer(segment_position))

# Create data frame with donor-recipient pairs and mutations matched         ---
df2 <- full_join(donor_mutations, recipient_mutations) %>% 
    full_join(donor_recipient) %>% 
    select(pair_id, donor, recipient, year, household, genome_segment,
           segment_position, donor_ref_allele, donor_alt_allele, donor_freq,
           recipient_ref_allele, recipient_alt_allele, recipient_freq) %>% 
    rename(donor_id = donor,
           recipient_id = recipient)

# Find the consensus allele for each sample at locations of interest         ---
donor_consensus <- df2 %>% 
    select(pair_id, donor_id, year, household, genome_segment, segment_position,
           donor_ref_allele) %>% 
    rename(full_id = donor_id) %>% 
    left_join(final_sequences_long) %>% 
    rename(donor_cons_allele = base,
           donor_id = full_id)

recipient_consensus <- df2 %>% 
    select(pair_id, recipient_id, year, household, genome_segment,
           segment_position, recipient_ref_allele) %>% 
    rename(full_id = recipient_id) %>% 
    left_join(final_sequences_long) %>% 
    rename(recipient_cons_allele = base,
           recipient_id = full_id)

# Put it all together in a single data frame                                 ---
df3 <- df2 %>% 
    full_join(recipient_consensus) %>% 
    full_join(donor_consensus) %>% 
    select(pair_id, donor_id, recipient_id, year, household,
           genome_segment, segment_position,
           donor_ref_allele, donor_cons_allele, 
           donor_alt_allele, donor_freq,
           recipient_ref_allele, recipient_cons_allele, 
           recipient_alt_allele, recipient_freq)

# Alter frequency readings according to consensus and alternative alleles    ---
### Define the conditions for future filtering
df4 <- df3 %>% 
    mutate(only_donor = ifelse(is.na(recipient_freq), TRUE, FALSE),
           only_recipient = ifelse(is.na(donor_freq), TRUE, FALSE),
           both = ifelse(!is.na(donor_freq) & !is.na(recipient_freq),
                         TRUE, FALSE),
           
           same_ref = ifelse(donor_ref_allele == recipient_ref_allele,
                             TRUE, FALSE),
           same_alt = ifelse(donor_alt_allele == recipient_alt_allele,
                             TRUE, FALSE),
           same_cons = ifelse(donor_cons_allele == recipient_cons_allele,
                              TRUE, FALSE),
           dCons_rAlt = ifelse(donor_cons_allele == recipient_alt_allele,
                               TRUE, FALSE),
           dAlt_rCons = ifelse(donor_alt_allele == recipient_cons_allele,
                               TRUE, FALSE))
df5 <- df4 %>% 
    mutate(donor_freq2 = ifelse(both == TRUE,
                                ifelse(same_cons == TRUE, donor_freq,
                                       ifelse(dCons_rAlt == TRUE,
                                              donor_freq, NA)),
                                ifelse(only_donor == TRUE,
                                       ifelse(same_cons == TRUE,
                                              (1 - donor_freq),
                                              ifelse(dAlt_rCons == TRUE,
                                                     donor_freq, NA)),
                                       ifelse(same_cons == TRUE,
                                              1,
                                              ifelse(dCons_rAlt == TRUE,
                                                     1, NA)))),
           recipient_freq2 = ifelse(both == TRUE,
                                    ifelse(same_cons == TRUE, recipient_freq,
                                           ifelse(dCons_rAlt == TRUE,
                                                  (1-recipient_freq), NA)),
                                    ifelse(only_donor == TRUE,
                                           ifelse(same_cons == TRUE, 1,
                                                  ifelse(dAlt_rCons == TRUE,
                                                         1, NA)),
                                           ifelse(same_cons == TRUE,
                                                  (1-recipient_freq),
                                                  ifelse(dCons_rAlt == TRUE,
                                                         recipient_freq, NA)))))

df_tv2 <- df4 %>% 
    mutate(donor_freq2 = ifelse(both == TRUE,
                                ifelse(same_cons == TRUE, donor_freq,
                                       ifelse(dCons_rAlt == TRUE,
                                              donor_freq, NA)),
                                ifelse(only_donor == TRUE, donor_freq, 0)),
           recipient_freq2 = ifelse(both == TRUE,
                                    ifelse(same_cons == TRUE, recipient_freq,
                                           ifelse(dCons_rAlt == TRUE,
                                                  (1-recipient_freq), NA)),
                                    ifelse(only_recipient == TRUE,
                                           recipient_freq, 0))) %>% 
    full_join(df5) %>% 
    anti_join(df_tv_nonZero) %>% 
    mutate(non_fixed2 = 1)

## TV plot with both arms ------------------------------------------------------
df_tv <- full_join(df_tv2, df_tv_nonZero) %>% 
    select(donor_freq2, recipient_freq2, non_fixed2, year, household) %>% 
    full_join(pair_meta, multiple = "all") %>% 
    mutate(pair_id = ifelse(is.na(pair_id), 22, pair_id))
    
household_num <- length(household_interest)

tv_plot <-  df_tv %>% 
    rename(donor = donor_freq2,
           recipient = recipient_freq2) %>% 
    ggplot(aes(x = donor, y = recipient)) +
    # scale_shape_manual(values = c(4:(household_num + 3), 20),
    #                    name = "Household\nNumber") +
    geom_point(aes(color = as.factor(non_fixed2)),
               size = 3, alpha = 0.5, shape = 20) +
    facet_wrap(~pair_id) +
    theme_bw(base_size = 16) +
    scale_x_continuous(expand = c(0.01,0.01), ) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    xlab("Frequency in Donor") +
    ylab("Frequency in Recipient") +
    scale_color_manual(values = c("black"), na.value = "red",
                       labels = c("Fixed OR\nBelow Theshold\n",
                                  "Non-Fixed AND\nAbove Threshold"),
                       name = "") +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
          axis.text.x = element_text(angle = 300, vjust = 0.3, hjust=0.3),
          panel.spacing = unit(2, "lines")) 

## TV plot - faceted -----------------------------------------------------------

colors <- c("#4E79A7", "#F28E2B", "#A0CBE8", "#A0CBE8", "#FFBE7D",
                     "#59A14F", "#8CD17D", "#B6992D", "#F1CE63", "#499894",
                     "#499894", "#86BCB6", "#E15759", "#FF9D9A", "#79706E",
                     "#BAB0AC", "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8",
                     "#9D7660", "#D7B5A6")

pairs_info <- df_tv %>% 
    select(pair_id, household) %>% 
    distinct() %>% 
    mutate(pair_id = ifelse(is.na(pair_id), 22, pair_id)) %>% 
    arrange(pair_id) %>% 
    mutate(pair_alpha = LETTERS[1:22],
           colors = colors)

pairs <- pairs_info %>% 
    select(pair_id) %>% 
    as_vector()

plot_list <- list()

for (i in pairs) {
    df <- df_tv %>% 
        filter(pair_id == i)
    plot_color <- pairs_info$colors[i]
    plot_title <- pairs_info$pair_alpha[i]
    plot <- df_facet %>% 
        rename(donor = donor_freq2,
               recipient = recipient_freq2) %>% 
        ggplot(aes(x = donor, y = recipient)) +
        geom_point(aes(color = as.factor(non_fixed2)),
                   size = 3, alpha = 0.5, shape = 20, show.legend = FALSE) +
        theme_classic(base_size = 7) +
        geom_rect(aes(xmin = 0, xmax = 1, ymin = 1.05, ymax = 1.2),
                  fill = plot_color) +
        ylim(c(0, 1.2)) +
        scale_x_continuous(expand = c(0.01,0.01)) +
        scale_y_continuous(expand = c(0, 0.05), breaks = seq(0, 1, by = 0.25)) +
        xlab("Donor") +
        ylab("Recipient") +
        scale_color_manual(values = c("black"), na.value = "red") +
        theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
              axis.text.x = element_text(angle = 180, vjust = 0.3, hjust=0.3)) +
        coord_capped_cart(left = 'top') +
        annotate('text', x = 0.50, y = 1.13, label = plot_title,
                 size = 3, fontface = "bold", color = "white") +   
        theme(plot.margin = unit(c(0,0,0,0), "cm"))
        
    plot_list[[i]] <- plot
}

tv_plot_facet <- cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                   plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                   plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]],
                   plot_list[[13]], plot_list[[14]], plot_list[[15]], plot_list[[16]],
                   plot_list[[17]], plot_list[[18]], plot_list[[19]], plot_list[[20]],
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
    # annotate("text", x = iSNV_sample_mean + 1, y = 20,
    #          label = paste("Mean = ",
    #                        round(iSNV_sample_mean, digits = 2), sep = ""),
    #          size = 6, col = "purple", hjust = 0) +
    # geom_vline(xintercept = iSNV_sample_median, col = "red") +
    # annotate("text", x = iSNV_sample_median + 1, y = 15,
    #          label = paste("Median = ",
    #                        round(iSNV_sample_median, digits = 2), sep = ""),
    #          size = 6, col = "red", hjust = 0) +
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
