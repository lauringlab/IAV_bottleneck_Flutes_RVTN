## Title: Descriptive Analysis of FluTES Data 
## Author: Katy Krupinsky
## Date Last Modified: 01/24/23
## Description: This script is the sampling scheme using the symptom onset date
## as the parameter for which to assign index participants.
##
## -----------------------------------------------------------------------------

## Start of old dataCleaning.R script ------------------------------------------
## Load packages and raw data --------------------------------------------------
library(tidyverse)
raw_data <- read.csv("./input/IAV_meta_snv.csv")

## Get data into form necessary for bottleneck calculation ---------------------
df1 <- raw_data %>% 
    # select relevant variables
    select(CaseID, hhsubid, REGION, POS, REF, ALT, mutation, ALT_FREQ_1,
           ALT_FREQ_2, mutation_type, index, PCR_RESULT_1, SPECDT_1, onsetdt,
           age.new, sex, p_seasvx, Year, household, Individual, collection_type,
           specimen_number, oder_of_sample, reference, hh_onsetdt, p_seasval1,
           p_seasval2, Days_post_symp_hh, avg_freq) %>% 
    
    # rename variables with code friendly names
    rename(full_id = CaseID,
           subj_id = hhsubid,
           genome_segment = REGION, 
           segment_position = POS,
           ref_allele = REF,
           alt_allele = ALT,
           mutation_lab = mutation, 
           alt_freq_1 = ALT_FREQ_1,
           alt_freq_2 = ALT_FREQ_2,
           flu_type = PCR_RESULT_1,
           collection_date = SPECDT_1,
           onset_date = onsetdt,
           age = age.new,
           vax_status = p_seasvx,
           year = Year,
           individual = Individual,
           order_of_sample = oder_of_sample,
           household_onset_date = hh_onsetdt,
           vax_year_1 = p_seasval1,
           vax_year_2 = p_seasval2,
           days_post_symptoms = Days_post_symp_hh) %>% 
    
    # convert formats of variables and re-level collection type so that it sorts
    # according to preference for self-collected samples
    mutate(collection_date = as.Date(collection_date, format = "%m/%d/%y"),
           onset_date = as.Date(onset_date, format = "%m/%d/%y"),
           collection_type = ifelse(collection_type == 0, 2, 1))

# create list of households which have more than 1 person who tested positive
household <- df1 %>% 
    group_by(year, household, subj_id) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(year, household) %>% 
    count() %>% 
    filter(n > 1) %>% 
    select(year, household)

# create list of samples which are the first collected for the individual
first_samples <- df1 %>% 
    ungroup() %>% 
    
    # determine the number of mutation per subject by collection type and date
    group_by(subj_id, collection_type, collection_date) %>% 
    arrange(subj_id, collection_type, collection_date) %>% 
    count() %>% 
    ungroup() %>% 
    
    # create our own sample number variable which takes into account collection
    # type
    group_by(subj_id) %>% 
    mutate(num = row_number()) %>% 
    
    # select for the first sample per individual of preferred collection type
    filter(num == 1) %>% 
    ungroup() %>% 
    
    # from this information, get the full_id for use in subsequent subsetting
    select(subj_id, collection_type, collection_date) %>% 
    inner_join(df1, multiple = "all") %>% 
    select(full_id) %>% 
    distinct()

# create data frame with only samples which are the first collected for the
# individual
df2 <- inner_join(df1, first_samples, multiple = "all")

# create data frame with only households with more than 1 person who tested
# positive
df3 <- inner_join(df2, household, multiple = "all") %>% 
    select(-(c(alt_freq_1, alt_freq_2))) %>% 
    select(-(c(genome_segment, segment_position, ref_allele, alt_allele))) %>% 
    ungroup() 

## Start of old samplingPlots.R script ## --------------------------------------
# Load packages ----------------------------------------------------------------
library(tidyverse)
library(ggpattern)
library(ggthemes)
library(paletteer)

df4 <- df1 %>% 
    select(full_id, subj_id, year, household, collection_date, 
           collection_type, index) %>% 
    distinct()

num_samples <- df4 %>%
    select(subj_id, collection_date) %>% 
    group_by(subj_id, collection_date) %>% 
    count() %>% 
    arrange(n) %>% 
    mutate(num_sample = ifelse(n == 1, NA, "two"))

index <- df4 %>% 
    select(subj_id, index) %>% 
    mutate(index2 = ifelse(index == 1, "Index", NA))

householdNames <- df4 %>% 
    select(year, household) %>% 
    distinct() %>% 
    arrange(year, household) %>% 
    group_by(year) %>% 
    mutate(new_name = row_number())

df5 <- df4 %>% 
    full_join(householdNames) %>% 
    full_join(num_samples) %>% 
    full_join(index, multiple = "all") %>% 
    ungroup() %>% 
    select(subj_id, year, household, collection_date, collection_type,
           index, index2, new_name, num_sample) %>% 
    arrange(year, household, num_sample, subj_id, collection_date) %>% 
    mutate(year_lab = ifelse(year == 17, "2017-2018", "2018-2019"))

household_colors <- rep(c("#4E79A7", "#A0CBE8"), 31)

df5 %>% 
    ggplot(aes(x = collection_date, y = as.factor(subj_id))) +
    geom_tile(aes(fill = as.factor(new_name)), color = "black") +
    geom_point(aes(col = index2)) +
    facet_wrap(~year_lab, scales = "free") +
    scale_fill_manual(values = household_colors) +
    scale_color_manual(values = "black", na.value = NA, name = "Index",
                       labels = c("", "")) +
    theme_hc(base_size = 12) +
    guides(fill = FALSE) +
    xlab("Collection Date") +
    ylab("Subject ID")

# select for only the first sample for each subject id -------------------------
df6 <- inner_join(first_samples, df4, multiple = "all") %>% 
    select(full_id, subj_id, year, household, collection_date, collection_type,
           index) %>% 
    distinct()

householdNames_2 <- df6 %>% 
    select(year, household) %>% 
    distinct() %>% 
    arrange(year, household) %>% 
    group_by(year) %>% 
    mutate(new_name = row_number())

df6 %>% 
    full_join(householdNames_2) %>% 
    full_join(index, multiple = "all") %>% 
    mutate(year_lab = ifelse(year == 17, "2017-2018", "2018-2019")) %>% 
    ggplot(aes(x = collection_date, y = as.factor(subj_id))) +
    geom_tile(aes(fill = as.factor(new_name)), color = "black") +
    geom_point(aes(col = index2)) +
    facet_wrap(~year_lab, scales = "free") +
    scale_fill_manual(values = household_colors) +
    scale_color_manual(values = "black", na.value = NA, name = "Index",
                       labels = c("", "")) +
    theme_hc(base_size = 12) +
    guides(fill = FALSE) +
    xlab("Collection Date") +
    ylab("Subject ID")

# filter out households which only have one sample -----------------------------
df7 <- inner_join(household, df6, multiple = "all") %>% 
    distinct()

householdNames_3 <- df7 %>% 
    select(year, household) %>% 
    distinct() %>% 
    arrange(year, household) %>% 
    group_by(year) %>% 
    mutate(new_name = row_number())

household_colors_3 <- rep(c("#4E79A7", "#A0CBE8"), 11)

df7 %>% 
    full_join(householdNames_3) %>% 
    inner_join(index, multiple = "all") %>% 
    mutate(year_lab = ifelse(year == 17, "2017-2018", "2018-2019")) %>% 
    ggplot(aes(x = collection_date, y = as.factor(subj_id))) +
    geom_tile(aes(fill = as.factor(new_name)), color = "black") +
    geom_point(aes(col = index2)) +
    facet_wrap(~year_lab, scales = "free") +
    scale_fill_manual(values = household_colors_3) +
    scale_color_manual(values = "black", na.value = NA, name = "Index",
                       labels = c("", "")) +
    theme_hc(base_size = 12) +
    guides(fill = "none") +
    xlab("Collection Date") +
    ylab("Subject ID")

# create a list of the samples we want to use ----------------------------------

final_ids <- df7 %>% 
    select(full_id)

# look at the mutations within each samples ------------------------------------

pairMeta <- read.table("./input/pair_meta.txt", header = TRUE) %>% 
    rename(pair = pair_id)

pairData <- pairMeta %>% 
    distinct() %>% 
    mutate(pair_alpha = LETTERS[1:22]) %>%
    rename(pair_id = pair) %>% 
    mutate(year = ifelse(is.na(year), 18, year)) %>% 
    rename(pair = pair_id)

rm(pairMeta)

df8 <- final_ids %>% 
    inner_join(df2, multiple = "all") %>% 
    mutate(exists = 1) %>% 
    ungroup()

genome_order <- final_ids %>% 
    inner_join(df2, multiple = "all") %>% 
    mutate(exists = 1) %>% 
    ungroup() %>% 
    select(exists, genome_segment, segment_position, mutation_lab) %>% 
    distinct() %>% 
    arrange(genome_segment, segment_position) %>% 
    mutate(mutation = as.factor(mutation_lab))

household_names_4 <- df8 %>% 
    select(household) %>% 
    distinct() %>% 
    mutate(new_name = row_number())

df9 <- full_join(df8, household_names_4) %>% 
    full_join(genome_order)

df9 %>% 
    ggplot(aes(x = mutation, y = as.factor(full_id))) +
    geom_tile(aes(fill = as.factor(new_name)), col = "black") +
    scale_fill_paletteer_d("ggthemes::Tableau_20", na.value = "white") +
    theme_hc() +
    xlab("Mutation Position") +
    ylab("Sample ID") +
    guides(fill = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_text(margin = margin(t = 20))) +
    geom_rug(aes(x = mutation, color = genome_segment), outside = TRUE,
             sides = "b") +
    scale_color_paletteer_d("ggthemes::Superfishel_Stone",
                            name = "Genome Segment") +
    coord_cartesian(clip = "off")

output_dataset <- df9

# final_ids <- output_dataset %>% 
#     select(full_id) %>% 
#     distinct() %>% 
#     as_vector()
# 
# 
# write.table(final_ids,
#             file = "./input/final_ids.txt",
#             sep = "\t",
#             row.names = FALSE, col.names = FALSE)
