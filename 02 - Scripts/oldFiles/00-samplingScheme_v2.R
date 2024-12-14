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
library(lubridate)
raw_data <- read.csv("/Users/katykrupinsky/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Flutes/SNV_with_meta_data.csv")

## Get data into form necessary for bottleneck calculation ---------------------
df1 <- raw_data %>% 
    # select relevant variables
    select(Study_ID, hhsubid, REGION, POS, REF, ALT, mutation, ALT_FREQ_1,
           ALT_FREQ_2, mutation_type, index, PCR_RESULT_1, SPECDT_1, onsetdt,
           age.new, sex, p_seasvx, Year, household, individual, collection_type,
           specimen_number, order_of_sample, reference, hh_onsetdt, 
           Days_post_symp_hh, avg_freq) %>% 
    
    # rename variables with code friendly names
    dplyr::rename(full_id = Study_ID,
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
           household_onset_date = hh_onsetdt,
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

householdNames <- df3 %>% 
    select(year, household) %>% 
    distinct() %>% 
    arrange(year, household) %>% 
    group_by(year) %>% 
    mutate(household_new = row_number())

df4 <- df3 %>% 
    full_join(householdNames) %>% 
    ungroup() %>% 
    select(full_id, subj_id, year, household_new, collection_date, onset_date, 
           collection_type, index) %>%
    arrange(year, household_new, subj_id, collection_date) %>% 
    mutate(year_lab = ifelse(year == 17, "2017-2018", 
                             ifelse(year == 18, "2018-2019", "2019-2020"))) %>% 
    mutate(not_prov = ifelse(is.na(onset_date), 1, 0),
         input_onset = ifelse(not_prov == 1, collection_date, onset_date)) %>% 
    mutate(input_onset = as_date(input_onset)) %>% 
    select(-c(not_prov, onset_date, collection_type)) %>% 
    arrange(year, household_new, input_onset)

df5 <- df4 %>% 
  select(full_id, subj_id, year, household_new, collection_date, year_lab, input_onset) %>% 
  distinct()

# create a list of the samples we want to use ----------------------------------

final_ids <- df5 %>% 
  select(full_id) %>% 
  distinct()

# figure out our list of donor/recipient pairings ------------------------------

years <- c("2017-2018", "2018-2019", "2019-2020")
n <- 1
list <- list()
for (y in 1:3) {
  raw_dat <- df5 %>% 
    filter(year_lab == years[[y]])
  households <- raw_dat %>% 
    select(household_new) %>% 
    distinct
  for (h in 1:nrow(households)){
  temp <- raw_dat %>% 
    filter(household_new == h)
  if (nrow(temp) == 2) {
      if (temp[1, 7] == temp[2, 7]) {
        donor <- temp %>% 
          select(full_id)
        recipient <- temp %>% 
          select(full_id) %>% 
          mutate(order = row_number()) %>% 
          arrange(-order) %>% 
          select(full_id)
        
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h) %>% 
          rename(donor = "full_id...1",
                 recipient = "full_id...2")
        
        list[[n]] <- out
        n <- n + 1
      } else {
        temp <- temp %>% 
          arrange(input_onset)
        donor <- temp[1, 1]
        recipient <- temp[2, 1]
        
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h) %>% 
          rename(donor = "...1",
                 recipient = "...2")
        
        list[[n]] <- out
        n <- n + 1
      }
    } else if (nrow(temp) == 3) {
      temp <- temp %>% 
        arrange(input_onset)
      if (temp[1, 7] != temp[2, 7] & temp[1, 7] != temp[2, 7]) {
        donor <- c(temp[1, 1], temp[1, 1]) %>% 
          as_tibble() %>% 
          rename(donor = value)
        recipient <- c(temp[2, 1], temp[3, 1]) %>% 
          as_tibble() %>% 
          rename(recipient = value)
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h)
        
        list[[n]] <- out
        n <- n + 1
      } else if (temp[1, 7] == temp[2, 7] & temp[1, 7] != temp[3, 7]) {
        donor <- c(temp[1, 1], temp[2, 1], temp [1, 1], temp[2, 1]) %>% 
          as_tibble() %>% 
          rename(donor = value)
        recipient <- c(temp[2, 1], temp[1, 1], temp[3, 1], temp[3, 1]) %>% 
          as_tibble() %>% 
          rename(recipient = value)
        
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h)
        
        list[[n]] <- out
        n <- n + 1
      } else if (temp[1, 7] == temp[2, 7] & temp[2, 7] == temp[3,7]) {
        donor <- c(temp[1, 1], temp[1, 1], temp[2, 1], temp[2,1], temp[3,1], temp[3, 1]) %>% 
          as_tibble() %>% 
          rename(donor = value)
        recipient <- c(temp[2, 1], temp[3, 1], temp[1, 1], temp[3, 1], temp[1, 1], temp[2, 1]) %>% 
          as_tibble() %>% 
          rename(recipient = value)
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h)
        
        list[[n]] <- out
        n <- n + 1
      } else {
        print("You have a household of 3 in a case you didn't deal with...do it!")
      }
    } else if (nrow(temp) == 4) {
      if (temp[1, 7] != temp[2, 7] & temp[1, 7] != temp[3, 7] & temp[1, 7] != temp[4, 7]) {
        donor <- rep(temp[1, 1], 3) %>% 
          as_tibble() %>% 
          rename(donor = value)
        recipient <- c(temp[2, 1], temp[3, 1], temp[4, 1]) %>% 
          as_tibble() %>% 
          rename(recipient = value)
        
        out <- bind_cols(donor, recipient) %>% 
          mutate(year = years[[y]],
                 household = h)
        
        list[[n]] <- out
        n <- n + 1
      } else {
        print("You have a household of 4 in a case you didn't deal with...do it!")
      }
    } else {
      print("You have a giant household, figure out how to deal with it!")
    }
  }
}

pairs <- bind_rows(list) %>% 
  mutate(pair = row_number()) %>% 
  rename(donor_id = donor,
         recipient_id = recipient,
         year = year,
         household = household) %>% 
  select(pair, donor_id, recipient_id, year, household) %>% 
  mutate(year = ifelse(year == "2017-2018", 17,
                       ifelse(year == "2018-2019", 18, 19)))

# look at the mutations within each samples ------------------------------------

# pairMeta <- read.table("./input/pair_meta.txt", header = TRUE) %>%
#     dplyr::rename(pair = pair_id)

pairData <- pairs

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

library(paletteer)
library(ggthemes)

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

rm(list=setdiff(ls(), c("output_dataset", "pairs")))

# Getting the thing in the right format for bottleneck input -------------------

pair_num <- pairs$pair
donor_id <- pairs$donor_id
recipient_id <- pairs$recipient_id

p <- 1
pair_num_loop <- pair_num[[p]]
donor_id_loop <- donor_id[[p]]
recipient_id_loop <- recipient_id[[p]]

donor_snv <- output_dataset %>% 
  filter(full_id == donor_id_loop) %>% 
  select(genome_segment, segment_position, ref_allele, alt_allele, avg_freq, reference) %>% 
  rename(donor_ref = ref_allele,
         donor_alt = alt_allele,
         donor_freq = avg_freq)
recipient_snv <- output_dataset %>% 
  filter(full_id == recipient_id_loop) %>% 
  select(genome_segment, segment_position, ref_allele, alt_allele, avg_freq, reference) %>% 
  rename(recipient_ref = ref_allele,
         recipient_alt = alt_allele,
         recipient_freq = avg_freq)
pair_out <- full_join(donor_snv, recipient_snv)

length_to_add <- read_csv("~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Consensus_sequence/Michigan_length_to_add.csv")

PB2_add <- length_to_add[1, 3] %>% 
  as_vector()
PB1_add <- length_to_add[2, 3] %>% 
  as_vector()
PA_add <- length_to_add[3, 3] %>% 
  as_vector()
HA_add <- length_to_add[4, 3] %>% 
  as_vector()
NP_add <- length_to_add[5, 3] %>% 
  as_vector()
NA__add <- length_to_add[6, 3] %>% 
  as_vector()
M_add <- length_to_add[7, 3] %>% 
  as_vector()
NS_add <- length_to_add[8, 3] %>% 
  as_vector()

positions <- pair_out %>% 
  select(genome_segment, segment_position) %>% 
  distinct() %>% 
  arrange(genome_segment, segment_position) %>% 
  mutate(position_adj = ifelse(genome_segment == "PB1",
                               segment_position + PB1_add,
                               ifelse(genome_segment == "PA",
                                      segment_position + PA_add,
                                      ifelse(genome_segment == "HA",
                                             segment_position + HA_add,
                                             ifelse(genome_segment == "NP",
                                                    segment_position + NP_add,
                                                    ifelse(genome_segment == "NA_",
                                                           segment_position + NA__add,
                                                           ifelse(genome_segment == "M",
                                                                  segment_position + M_add,
                                                                  ifelse(genome_segment == "NS",
                                                                         segment_position + NS_add, segment_position)))))))) %>% 
  select(position_adj) %>% 
  as_vector()

write.(positions, "Michigan_positions.csv", row.names = FALSE)


Michigan_official <- Michigan_2017_clean %>% 
  filter(full_id == 1801301001) %>% 
  mutate(position_adj = ifelse(genome_segment == "PB1",
                               segment_position + PB1_add,
                               ifelse(genome_segment == "PA",
                                      segment_position + PA_add,
                                      ifelse(genome_segment == "HA",
                                             segment_position + HA_add,
                                             ifelse(genome_segment == "NP",
                                                    segment_position + NP_add,
                                                    ifelse(genome_segment == "NA_",
                                                           segment_position + NA__add,
                                                           ifelse(genome_segment == "M",
                                                                  segment_position + M_add,
                                                                  ifelse(genome_segment == "NS",
                                                                         segment_position + NS_add, segment_position)))))))) %>% 
  filter(position_adj == 6017)
  
 






