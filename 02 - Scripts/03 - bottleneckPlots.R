## Title: Bottleneck Plots
## Author: Katy Krupinsky
## Date Last Modified: 02/22/24
## Description: This script takes the output from the BottleneckCalculation.R
## script and creates data visualizations.
##
## -----------------------------------------------------------------------------

## Load packages and raw data --------------------------------------------------
library(tidyverse)
library(ggthemes)
library(paletteer)
library(ggnewscale)
library(ggforce)
library(ggpattern)
library(janitor)

pairMeta <- read.table("./input2/pair_meta.txt", header = TRUE)
varientCounts <- read.table("./bottleneckOutput3/all_zero.txt", header = TRUE) %>% 
  dplyr::rename(pair_id = pair)

pairData <- full_join(pairMeta, varientCounts) %>% 
  distinct() %>%
  ungroup() %>% 
  group_by(year, household)

rm(pairMeta)
rm(varientCounts)

householdNew <- pairData %>% 
  select(year, household) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(household_new = LETTERS[row_number()])

pairNew <- pairData %>% 
  ungroup() %>% 
  select(year, pair_id, household, n_variants) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(year, household) %>% 
  arrange(year, household, n_variants) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(pair_id_new = row_number())

pairDataNew <- full_join(pairData, householdNew) %>% 
  full_join(pairNew)

rm(pairData)
rm(pairNew)
rm(householdNew)

all_idsA <- c(pairDataNew$donor_id, pairDataNew$recipient_id) %>% 
  as.data.frame() %>% 
  distinct()

## Do the typical epidemiology graph thing -------------------------------------

#A - Look at the age of the donor and recipient
pairDataNew %>% 
  ggplot() +
  geom_density(aes(x = donor_age), fill = "purple", alpha = 0.5) +
  geom_density(aes(x = recipient_age), fill = "blue", alpha = 0.5) +
  theme_light(base_size = 18)

## Create plot of number of variants used within bottleneck calculation --------

pairDataNew %>% 
  mutate(year = ifelse(year == 17, "Flutes\n2017/2018",
                       ifelse(year == 18, "Flutes\n2018/2019", 
                              ifelse(year == 19, "Flutes\n2019/2020", "RVTN\n2021/2022")))) %>% 
  ggplot() +
  geom_col(aes(x = as.factor(pair_id_new), y = n_variants,
               fill = as.factor(household_new), col = as.factor(strain)),
           linewidth = 1) +
  theme_hc(base_size = 18) +
  scale_fill_paletteer_d("ggthemes::Tableau_20", name = "Household") +
  scale_color_manual(values = c("black", "white"), labels = c("H1N1", "")) +
  xlab("Pair Number") +
  ylab("Number of Varients Used in Bottleneck Calculation") +
  labs(col = "Strain") +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white")) +
  facet_wrap(~year, nrow = 3, scales = "free_y") +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

pairDataNew %>% 
  mutate(year = ifelse(year == 17, "Flutes\n2017/2018",
                       ifelse(year == 18, "Flutes\n2018/2019", 
                              ifelse(year == 19, "Flutes\n2019/2020", "RVTN\n2021/2022")))) %>%
  ggplot() +
  geom_histogram(aes(x = n_variants), fill = "blue", alpha = 0.5) +
  facet_wrap(vars(year))

## Import output from bottleneckOutput.R script --------------------------------

pairs <- read.table("./bottleneckOutput3/all_zero.txt") %>% 
    filter(V2 == FALSE) %>% 
    select(V1) %>% 
    mutate(V1 = as.character(V1)) %>% 
    as_vector()

list <- vector("list")

for (i in pairs){
    filename <- paste("./bottleneckOutput3/",
                      paste(paste("output_pair", i, sep = "_"), ".txt",
                            sep = ""),
                      sep = "")
    objectname <- paste("pair_", i, sep = "")
    df <- read.table(filename, sep = "\t") %>% 
        mutate(pair = as.character(i))
    list[[i]] <- df
}

all_pairs <- bind_rows(list)

confidence <- read.table("./bottleneckOutput3/confidence_int.txt") %>% 
  row_to_names(row_number = 1) %>% 
    na.omit() %>% 
    mutate(pair_id = as.double(pair))

full_merged <- confidence %>% 
  full_join(pairDataNew)

## Plot MLE for each individual transmission pair ------------------------------

df <- bind_rows(list) %>% 
    dplyr::rename(bottleneck_size = V1,
           log_liklihood = V2) %>% 
    full_join(confidence) %>% 
    mutate(max_LL = as.numeric(max_LL),
           upper_CI = as.numeric(upper_CI),
           lower_CI = as.numeric(lower_CI)) %>% 
    inner_join(pairDataNew) %>% 
  mutate(pair_vax = ifelse(donor_vax == 1 & recipient_vax == 1, "Both",
                           ifelse(donor_vax == 1 & recipient_vax == 0, "Donor Only",
                                  ifelse(donor_vax == 0 & recipient_vax == 1, "Recipient Only",
                                         "Neither")))) %>% 
  mutate(donor_age_dicot = ifelse(donor_age < 18, "Child", "Adult"),
         recipient_age_dicot = ifelse(recipient_age < 18, "Child", "Adult")) %>% 
  mutate(pair_age = ifelse(donor_age_dicot == "Child" & recipient_age_dicot == "Child", "Child-Child",
                           ifelse(donor_age_dicot == "Adult" & recipient_age_dicot == "Adult", "Adult-Adult",
                                  ifelse(donor_age_dicot == "Child" & recipient_age_dicot == "Adult", "Child2Adult",
                                         "Adult2Child"))))

temp <- confidence %>% 
    inner_join(pairDataNew) %>%
  mutate(lower_CI = as.numeric(lower_CI),
         upper_CI = as.numeric(upper_CI),
         max_LL = as.numeric(max_LL)) %>% 
  mutate(pair_vax = ifelse(donor_vax == 1 & recipient_vax == 1, "Both",
                           ifelse(donor_vax == 1 & recipient_vax == 0, "Donor Only",
                                  ifelse(donor_vax == 0 & recipient_vax == 1, "Recipient Only",
                                         "Neither")))) %>% 
  mutate(donor_age_dicot = ifelse(donor_age < 18, "Child", "Adult"),
         recipient_age_dicot = ifelse(recipient_age < 18, "Child", "Adult")) %>% 
  mutate(pair_age = ifelse(donor_age_dicot == "Child" & recipient_age_dicot == "Child", "Child-Child",
                           ifelse(donor_age_dicot == "Adult" & recipient_age_dicot == "Adult", "Adult-Adult",
                                  ifelse(donor_age_dicot == "Child" & recipient_age_dicot == "Adult", "Child2Adult",
                                         "Adult2Child"))))

temp %>%
  filter(pair_id != 48) %>% 
  mutate(year = ifelse(year == 17, "Flutes\n2017/2018",
                       ifelse(year == 18, "Flutes\n2018/2019", 
                              ifelse(year == 19, "Flutes\n2019/2020", "RVTN\n2021/2022")))) %>% 
  ggplot() +
  geom_col(aes(x = as.factor(pair_id_new), y = max_LL, 
               fill = as.factor(household_new), col = as.factor(strain))) +
  geom_errorbar(aes(x = as.factor(pair_id_new), ymin = lower_CI, ymax = upper_CI)) +
  scale_fill_paletteer_d("ggthemes::Tableau_20",
                         name = "Household") +
  facet_wrap(~year, nrow = 4, scales = "free_y") +
  # ggforce::facet_zoom(ylim = c(0, 5)) +
  scale_color_manual(values = c("black", "white"), labels = c("H1N1", "")) +
  theme_hc(base_size = 18) +
  labs(col = "Strain") +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white")) +
  xlab("Transmission Pair") +
  ylab("Bottleneck Size")  +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

temp %>% 
  ggplot() +
  geom_histogram(aes(x = max_LL, fill = as.factor(strain)), binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  facet_wrap(vars(year)) +
  theme_bw(base_size = 18)

temp %>% 
  ggplot() +
  geom_boxplot(aes(y = max_LL, x = pair_age, fill = pair_vax)) +
  coord_cartesian(ylim = c(0, 25))

## Calculate the overall bottleneck size ---------------------------------------

# A - Filter out our problem child pair
bad_pairs <- df %>%
  filter(year == 19) %>%
  filter(n_variants > 20) %>%
  select(pair_id) %>%
  distinct() %>%
  as_vector()

df_temp <- df %>%
  filter(pair_id != bad_pairs)

i <- 1
y <- 0000

years <- c(17, 18, 19, 21)
strains <- c("H1N1", "H3N2")
list3 <- list()
list4 <- list()


for (a in 1:4){
  y <- years[a]
for (b in 1:2) {
  s <- strains[b]
    
    df2 <- df %>% 
      filter(year == y) %>%
      filter(strain == s) 
    if (nrow(df2) != 0){
    max_num_snps <- max(df2$n_variants)
    
    total_LL <- df2 %>%
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
    
    out <- data.frame(max_LL = Max_LL_bottleneck, 
                      lower_CI = lower_CI_bottleneck,
                      upper_CI = upper_CI_bottleneck,
                      year = y,
                      strain = s)
    
    list3[[i]] <- out
    list4[[i]] <- total_LL %>% 
      mutate(year = y,
             strain = s)
    i <- i + 1
    }
}
}

all_bottlenecks <- bind_rows(list3)
all_total_LL <- bind_rows(list4)

## Now calculate it depending on the type of pairing ---------------------------
i <- 1
y <- 0000

years <- c(17, 18, 19, 21)
strains <- c("H1N1", "H3N2")
pairings_age <- c("Adult-Adult", "Adult2Child", "Child-Child", "Child2Adult")
pairings_vax <- c("Both", "Donor Only", "Neither", "Recipient Only")
list3 <- list()
list4 <- list()


for (a in 1:4){
  y <- years[a]
  for (b in 1:2) {
    s <- strains[b]
    for (a in 1:4) {
    pair_age <- pairings_age[a]
      for (v in 1:4) {
        pair_vax <- pairings_vax[v]
    df2 <- df %>% 
      filter(year == y) %>%
      filter(strain == s) %>% 
      filter(pair_age == pair_age) %>% 
      filter(pair_vax == pair_vax)
    if (nrow(df2) != 0){
      max_num_snps <- max(df2$n_variants)
      
      total_LL <- df2 %>%
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
      
      out <- data.frame(max_LL = Max_LL_bottleneck, 
                        lower_CI = lower_CI_bottleneck,
                        upper_CI = upper_CI_bottleneck,
                        year = y,
                        strain = s,
                        pair_vax = pair_vax,
                        pair_age = pair_age)
      
      list3[[i]] <- out
      list4[[i]] <- total_LL %>% 
        mutate(year = y,
               strain = s)
      i <- i + 1
    }
  }
    }
  }
}

all_bottlenecks <- bind_rows(list3)
# all_total_LL <- bind_rows(list4)

all_bottlenecks %>% 
  ggplot(aes(y = max_LL, x = pair_age, col = pair_vax)) +
  geom_jitter() +
  facet_wrap(vars(year))

# ## Plot overall bottleneck size ------------------------------------------------
# 
# all_total_LL %>% 
#   filter(adjusted_LL != -Inf) %>% 
#   bind_rows(all_bottlenecks) %>%
#   ggplot() +
#   geom_point(aes(x = bottleneck_size, y = adjusted_LL), col = "#5C6068") +
#   geom_vline(mapping = aes(xintercept = max_LL), col = "#E15759",
#              lwd = 1) +
#   geom_vline(aes(xintercept = lower_CI), col = "#4E79A7", lty = 2,
#              lwd = 1) +
#   geom_vline(aes(xintercept = upper_CI), col = "#4E79A7", lty = 2,
#              lwd = 1) +
#   facet_grid(vars(year), vars(strain), scales = "free_y") +
#   theme_bw(base_size = 18) +
#   xlab("Bottleneck Size") +
#   ylab("Log-Likelihood") +
#   theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
