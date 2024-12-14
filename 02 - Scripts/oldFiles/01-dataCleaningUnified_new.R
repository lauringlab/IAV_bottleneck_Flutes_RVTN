## Title: Creation of Input Files for Bottleneck Calculations
## Date Last Modified: 02/22/24
## Description: This code takes the unified metadata files provided by CDC in
## 2023 and merges them with the SNV data created by the lab. It then will pull
## the consensus sequence data for the 17-18, 18-19, and 19-20 seasons (21-22
## season will be added soon) and add it to the overall dataframe. Finally,
## it will create files necessary for both doing the bottleneck calculation
## using the file titled "02-bottleneckCalculation.R" and some of the files
## necessary to create the plots using the file titled "03-bottleneckPlots.R".
##
## -----------------------------------------------------------------------------

files.sources <-list.files(path = "./01 - Functions", full.names = TRUE)
sapply(files.sources, source)

## Calling with 0.02 threshold

path_to_fulldat <- "/Users/katykrupinsky/Dropbox (University of Michigan)/CDC_RVTN_FLUTES_Flu/fulldat_lauring_flu2023-11-21.csv"
path_to_ddlabdat <- "/Users/katykrupinsky/Dropbox (University of Michigan)/CDC_RVTN_FLUTES_Flu/ddlabdat_lauring_flu2023-11-21.csv"
path_to_flutes_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/Flutes/SNV_with_meta_data.csv"
path_to_rvtn_snv <- "~/Dropbox (University of Michigan)/Flu_bottleneck_Flutes_RVTN/RVTN/SNV_with_mutation_type.csv"

df <- joinMetadata(path_to_fulldat = path_to_fulldat, path_to_ddlabdat = path_to_ddlabdat)
flutes_unified <- joinFlutesSnvToMeta(df, path_to_flutes_snv = path_to_flutes_snv)
rvtn_unified <- joinRvtnSnvToMeta(df, path_to_rvtn_snv = path_to_rvtn_snv)

# 10. Put the whole shabang together finally 
df1 <- bind_rows(rvtn_unified, flutes_unified) %>% 
  select(-c(cdc_flupos_sample, GFF_FEATURE, REF_CODON, ALT_CODON, sample_mutation))

# 11. Do some data cleaning on this dataset now that we have it all together
df2 <- df1 %>% 
  ### grab the things we care about
  select(sample, cdc_studyid, cdc_hhid, cdc_site, season, age_at_enrollment, age.new, sex, 
         season_vax, p_seasvx, first_pos, symptomatic, symptom_start, cdc_flu_type2, rvtn,
         date, REGION, POS, REF, ALT, mutation_type, avg_freq) %>% 
  arrange(season) %>% 
  ### if there is no season, pull it from the sample id
  mutate(season_new = ifelse(is.na(season), substr(sample, 1, 2), substr(season, 1, 2))) %>% 
  select(-season) %>% 
  dplyr::rename(season = season_new) %>% 
  mutate(season = as.numeric(season),
         ### clean up the age column so that all the information is in one place
         age_imput = ifelse(is.na(age.new), age_at_enrollment, age.new)) %>% 
  mutate(age = as.numeric(age_imput)) %>% 
  ### once again only grabing things we care about and putting them in a useful order
  select(sample, cdc_studyid, cdc_hhid, cdc_site, season, age, sex, 
         season_vax, p_seasvx, first_pos, symptomatic, symptom_start, cdc_flu_type2, rvtn,
         date, REGION, POS, REF, ALT, mutation_type, avg_freq) %>% 
  ## clean up the vax column so that all the information is in one place
  mutate(vax = ifelse(is.na(season_vax), p_seasvx, season_vax)) %>% 
  ## put everything in a single format so we don't have to do all of this weird stuff
  mutate(vax = ifelse(vax == "Unvaccinated", 0,
                      ifelse(vax == "Vaccinated", 1,
                             ifelse(vax == "Recent vaccination or unknown", 99, vax)))) %>% 
  mutate(vax = as.numeric(vax)) %>% 
  # filter(vax != 99) %>% 
  ## get rid of variables which we don't necessarily need
  select(-c(season_vax, p_seasvx)) %>% 
  arrange(season) %>% 
  ## create a column which will allow us to do the necessary id cleaning
  mutate(flutes = ifelse(season < 20, 1, 0)) %>% 
  ## once again reorder our columns
  select(sample, season, age, sex, vax, first_pos, symptomatic, symptom_start,
         cdc_flu_type2, flutes, date, REGION, POS, REF, ALT, mutation_type, avg_freq) %>% 
  ## pull out household and person data in preparation for joining with consensus information
  mutate(household = ifelse(flutes == 1, substr(sample, 3, 5), substr(sample, 2, 4)),
         person = ifelse(flutes == 1, substr(sample, 6, 7), substr(sample, 5, 6))) %>% 
  mutate(household = as.numeric(household),
         person = as.numeric(person)) %>% 
  ## final reordering to not drive Katy a little crazy 
  select(sample, season, household, person, age, sex, vax, cdc_flu_type2, 
         flutes, date, REGION, POS, REF, ALT, mutation_type, avg_freq)

# TEST CODE TO FILTER OUT THOSE WITH JUST A TON OF SNVS
num_snv <- df2 %>% 
  ungroup() %>% 
  group_by(sample) %>% 
  count() %>% 
  filter(n < 50) %>% 
  select(sample)

df2 <- df2 %>% 
  right_join(num_snv)

# 12. Bring in our consensus information
source("~/Documents/College/03-UM/Research/Aim 2/scripts/00-consensusSequences.R")

# rm(list= ls()[!(ls() %in% c('df2', 'all_cons'))])

# 13. add in the consensus information with the snvs
df3 <- df2 %>% 
  left_join(all_cons, multiple = "all") %>% 
  mutate(strain_type = str_sub(strain, -4, -1)) %>% 
  mutate(site = ifelse(flutes == 0, str_sub(sample, 1, 1), 1)) %>% 
  select(flutes, sample, site, season, household, person, age, sex, vax, strain_type, date,
         REGION, POS, REF, ALT, CONS, avg_freq, mutation_type, strain)

# 14. Determine the transmission pairs -----------------------------------------
## A - pull out the households which are sequenced and have more than one person
households <- df3 %>% 
  select(sample, site, season, household, person, age, sex, strain_type, date) %>% 
  distinct() %>% 
  group_by(site, season, household) %>% 
  count() %>% 
  filter(n > 1)

## B - grab the ids which are within those final households 
final_ids <- df3 %>% 
  select(sample, site, season, household, person, date, strain_type, vax, age, sex) %>%
  distinct() %>% 
  right_join(households) %>% 
  arrange(season, site, household, date) %>% 
  select(-n, -person)

temp <- final_ids %>% 
  left_join(df3) %>% 
  mutate(weird = ifelse(REF != CONS, 1, 0)) %>%
  filter(weird == 1)

if(nrow(temp) >=1) {
  print("WARNING!!!!! THE CONSENSUS THING IS HAPPENING AGAIN!!! :(")
}

final_pairs <- createPairs(sites, seas_house, pair)

# 15. Create things to make the plotting script actually work ------------------
pair_meta_out <- final_pairs %>% 
  dplyr::rename(donor_id = donor,
                recipient_id = recipient,
                year = season) %>% 
  select(pair_id, donor_id, recipient_id, year, site, household, strain,
         donor_vax, recipient_vax, donor_age, recipient_age)

write.table(pair_meta_out,
            file = "/Users/katykrupinsky/Documents/College/03-UM/Research/Lauring Lab/Bottlenecks/input/pair_meta.txt",
            sep = "\t",
            row.names = FALSE, col.names = TRUE)
pairs <- 1:nrow(pair_meta_out) %>% 
  data.frame()

write.table(pairs,
            file = "./input/pairs.txt",
            sep = "\t",
            row.names = FALSE, col.names = FALSE)

# 16. Create the files which go into the bottleneck calculation ----------------
list2 <- list()

for(p in 1:nrow(final_pairs)){
  ### isolate the pair the loop is on
  temp <- final_pairs %>% 
    filter(pair_id == p)
  
  ### pull out donor and recipient metadata
  donor_meta <- df3 %>% 
    filter(sample == temp$donor[1]) %>% 
    dplyr::rename(donor_sample = sample,
                  donor_age = age,
                  donor_sex = sex,
                  donor_vax = vax,
                  donor_date = date) %>% 
    select(donor_sample, donor_age, donor_sex, donor_vax, donor_date) %>% 
    distinct()
  
  recipient_meta <- df3 %>% 
    filter(sample == temp$recipient[1]) %>% 
    dplyr::rename(recipient_sample = sample,
                  recipient_age = age,
                  recipient_sex = sex,
                  recipient_vax = vax,
                  recipient_date = date) %>% 
    select(recipient_sample, recipient_age, recipient_sex, recipient_vax, recipient_date) %>% 
    distinct()
  
  ### pull out donor and recipient snv
  donor_snv <- df3 %>% 
    filter(sample == temp$donor[1]) %>% 
    dplyr::rename(donor_ref = REF,
                  donor_alt = ALT,
                  donor_cons = CONS, 
                  donor_freq = avg_freq) %>% 
    select(REGION, POS, donor_ref, donor_alt, donor_cons, donor_freq) %>% 
    ### do a somewhat clever thing to get the corrosponding recipient consensus data for the donor snvs
    mutate(sample = temp$recipient[1]) %>% 
    left_join(all_cons) %>% 
    dplyr::rename(recip_cons2 = CONS) %>% 
    select(-sample)
  
  recip_snv <- df3 %>% 
    filter(sample == temp$recipient[1]) %>% 
    dplyr::rename(recip_ref = REF,
                  recip_alt = ALT,
                  recip_cons = CONS, 
                  recip_freq = avg_freq) %>% 
    select(REGION, POS, recip_ref, recip_alt, recip_cons, recip_freq) %>% 
    ### do a somewhat clever thing to get the corrosponding donor consensus data for the donor snvs
    mutate(sample = temp$donor[1]) %>% 
    left_join(all_cons) %>% 
    dplyr::rename(donor_cons2 = CONS) %>% 
    select(-sample)
  
  ### put it all together and make sure that the column names are all gucci
  pair_snv <- full_join(donor_snv, recip_snv) %>% 
    mutate(pair_id = p) %>% 
    mutate(donor_CONS = ifelse(is.na(donor_cons), donor_cons2, donor_cons),
           recip_CONS = ifelse(is.na(recip_cons), recip_cons2, recip_cons)) %>% 
    mutate(donor_sample = donor_meta$donor_sample[1],
           donor_age = donor_meta$donor_age[1],
           donor_sex = donor_meta$donor_sex[1],
           donor_vax = donor_meta$donor_vax[1],
           donor_date = donor_meta$donor_date[1],
           recipient_sample = recipient_meta$recipient_sample[1],
           recipient_age = recipient_meta$recipient_age[1],
           recipient_sex = recipient_meta$recipient_sex[1],
           recipient_vax = recipient_meta$recipient_vax[1],
           recipient_date = recipient_meta$recipient_date[1]) %>% 
    mutate(season = temp$season[1],
           household = temp$household[1]) %>% 
    select(pair_id, season, household, donor_sample, recipient_sample,
           donor_age, recipient_age, donor_sex, recipient_sex, 
           donor_vax, recipient_vax, donor_date, recipient_date,
           REGION, POS, donor_ref, recip_ref, donor_alt, recip_alt,
           donor_CONS, recip_CONS, donor_freq, recip_freq)
  
  ### do the necessary corrections in order to make the bottleneck thing work
  pair_snv2 <- pair_snv %>%
    mutate(only_donor = ifelse(is.na(recip_freq), TRUE, FALSE),
           only_recipient = ifelse(is.na(donor_freq), TRUE, FALSE),
           both = ifelse(!is.na(donor_freq) & !is.na(recip_freq),
                         TRUE, FALSE),
           same_ref = ifelse(donor_ref == recip_ref,
                             TRUE, FALSE),
           same_alt = ifelse(donor_alt == recip_alt,
                             TRUE, FALSE),
           same_cons = ifelse(donor_CONS == recip_CONS,
                              TRUE, FALSE),
           dCons_rAlt = ifelse(donor_CONS == recip_alt,
                               TRUE, FALSE),
           dAlt_rCons = ifelse(donor_alt == recip_CONS,
                               TRUE, FALSE))
  
  pair_snv3 <- pair_snv2 %>%
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
           recip_freq2 = ifelse(both == TRUE,
                                ifelse(same_cons == TRUE, recip_freq,
                                       ifelse(dCons_rAlt == TRUE,
                                              (1-recip_freq), NA)),
                                ifelse(only_donor == TRUE,
                                       ifelse(same_cons == TRUE, 1,
                                              ifelse(dAlt_rCons == TRUE,
                                                     1, NA)),
                                       ifelse(same_cons == TRUE,
                                              (1-recip_freq),
                                              ifelse(dCons_rAlt == TRUE,
                                                     recip_freq, NA)))))
  list2[[p]] <- pair_snv3
  
  # # UNCOMMENT THIS IF YOU WANT TO SAVE ALL THAT WORK
  # ### all that work, and this is what we actually want at the end of the day
  # pair_export <- pair_snv3 %>%
  #   select(donor_freq2, recip_freq2)
  # 
  # ### export it...FINALLY
  # file_name <- paste("./processedData/",
  #                    paste(paste("pair", p, sep = "_"), ".txt", sep = ""),
  #                    sep = "")
  # write.table(pair_export,
  #             file = file_name,
  #             sep = "\t",
  #             row.names = FALSE, col.names = FALSE)
}

all_snv <- bind_rows(list2)
write.table(all_snv,
            file = "./totalsnv.txt",
            sep = "\t",
            row.names = FALSE, col.names = TRUE)
