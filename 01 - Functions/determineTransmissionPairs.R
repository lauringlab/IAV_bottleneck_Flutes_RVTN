#NOTE: This is a less efficient versions of determineTransmissionPairs2. Use
#that one instead!

determineTransmissionPairs <- function(df3) {
  
  ## B - Pull out only the first samples that we have for each individual
  flutes_snv_B <- flutes_snv %>%
    arrange(year, household, individual, date, collection_type) %>%
    group_by(Year, household, individual) %>%
    mutate(sample_num_new = row_number()) %>%
    filter(sample_num_new == 1) %>%
    arrange(Study_ID) %>%
    left_join(flutes_snv) %>%
    mutate(date = mdy(SPECDT_1)) %>%
    select(-index, -sex)  
  
  ## A - pull out the households which are sequenced and have more than one person
    households <- df3 %>%
        select(sample,
               site,
               season,
               household,
               person,
               age,
               sex,
               strain_type,
               date) %>%
        distinct() %>%
        group_by(site, season, household) %>%
        count() %>%
        filter(n > 1)
    
    ## B - grab the ids which are within those final households
    final_ids <- df3 %>%
        select(sample,
               site,
               season,
               household,
               person,
               date,
               strain_type,
               vax,
               age) %>%
        distinct() %>%
        right_join(households) %>%
        arrange(season, site, household, date) %>%
        select(-n, -person)
    
    temp <- final_ids %>%
        left_join(df3) %>%
        mutate(weird = ifelse(REF != CONS, 1, 0)) %>%
        filter(weird == 1)
    
    if (nrow(temp) >= 1) {
        print("WARNING!!!!! THE CONSENSUS THING IS HAPPENING AGAIN!!! :(")
    }
    
    # C - pull out our actual transmission pairs
    seasons <- c(17, 18, 19, 21)
    list <- list()
    n <- 1
    s <- 1
    h <- 1
    t <- 1
    
    for (s in 1:length(seasons)) {
        seas <- seasons[s]
        sites <- final_ids %>%
            filter(season == seas) %>%
            select(site) %>%
            distinct() %>%
            as_vector()
        
        for (t in 1:length(sites)) {
            sit <- sites[t]
            seas_house <- final_ids %>%
                filter(season == seas, site == sit) %>%
                select(household) %>%
                distinct() %>%
                as_vector()
            
            for (h in 1:length(seas_house)) {
                house <- seas_house[h]
                pair <- final_ids %>%
                    filter(season == seas, household == house, site == sit) %>%
                    arrange(date)
                
                if (nrow(pair) == 2) {
                    if (pair$strain_type[1] != pair$strain_type[2] |
                        abs(pair$date[1] - pair$date[2]) > 14) {
                        print("This is likely not a real pair")
                    } else {
                        if (pair$date[1] != pair$date[2]) {
                            donor <- pair$sample[1]
                            recipient <- pair$sample[2]
                            donor_vax <- pair$vax[1]
                            recipient_vax <- pair$vax[2]
                            donor_age <- pair$age[1]
                            recipient_age <- pair$age[2]
                            
                            out <- data.frame(donor, recipient) %>%
                                mutate(
                                    season = seas,
                                    site = sit,
                                    household = house,
                                    strain = pair$strain_type[1],
                                    donor_vax = donor_vax,
                                    recipient_vax = recipient_vax,
                                    donor_age = donor_age,
                                    recipient_age = recipient_age
                                ) %>%
                                select(
                                    season,
                                    site,
                                    household,
                                    donor,
                                    recipient,
                                    strain,
                                    donor_vax,
                                    recipient_vax,
                                    donor_age,
                                    recipient_age
                                )
                        } else {
                            donor <- c(pair$sample[1], pair$sample[2])
                            recipient <- c(pair$sample[2], pair$sample[1])
                            donor_vax <- c(pair$vax[1], pair$vax[2])
                            recipient_vax <- c(pair$vax[2], pair$vax[1])
                            donor_age <- c(pair$age[1], pair$age[2])
                            recipient_age <- c(pair$age[2], pair$age[1])
                            
                            out <- data.frame(donor, recipient) %>%
                                mutate(
                                    season = seas,
                                    site = sit,
                                    household = house,
                                    strain = pair$strain_type[1],
                                    donor_vax = donor_vax,
                                    recipient_vax = recipient_vax,
                                    donor_age = donor_age,
                                    recipient_age = recipient_age
                                ) %>%
                                select(
                                    season,
                                    site,
                                    household,
                                    donor,
                                    recipient,
                                    strain,
                                    donor_vax,
                                    recipient_vax,
                                    donor_age,
                                    recipient_age
                                )
                        }
                    }
                } else if (nrow(pair) == 3) {
                    if (pair$strain_type[1] != pair$strain_type[2] |
                        pair$strain_type[1] != pair$strain_type[3] |
                        pair$strain_type[2] != pair$strain_type[3] |
                        abs(pair$date[1] - pair$date[2]) > 14 |
                        abs(pair$date[1] - pair$date[3]) > 14 |
                        abs(pair$date[3] - pair$date[2]) > 14) {
                        print("This is likely not a real pair")
                    } else {
                        if (pair$date[1] < pair$date[2] & pair$date[1] < pair$date[3]) {
                            donor <- c(pair$sample[1], pair$sample[1])
                            recipient <- c(pair$sample[2], pair$sample[3])
                            donor_vax <- c(pair$vax[1], pair$vax[1])
                            recipient_vax <- c(pair$vax[2], pair$vax[3])
                            donor_age <- c(pair$age[1], pair$age[1])
                            recipient_age <- c(pair$age[2], pair$age[3])
                            
                            out <- data.frame(donor, recipient) %>%
                                mutate(
                                    season = seas,
                                    site = sit,
                                    household = house,
                                    strain = pair$strain_type[1],
                                    donor_vax = donor_vax,
                                    recipient_vax = recipient_vax,
                                    donor_age = donor_age,
                                    recipient_age = recipient_age
                                ) %>%
                                select(
                                    season,
                                    site,
                                    household,
                                    donor,
                                    recipient,
                                    strain,
                                    donor_vax,
                                    recipient_vax,
                                    donor_age,
                                    recipient_age
                                )
                            
                        } else if (pair$date[1] == pair$date[2] &
                                   pair$date[2] == pair$date[3]) {
                            donor <- c(
                                pair$sample[1],
                                pair$sample[1],
                                pair$sample[2],
                                pair$sample[2],
                                pair$sample[3],
                                pair$sample[3]
                            )
                            recipient <- c(
                                pair$sample[2],
                                pair$sample[3],
                                pair$sample[1],
                                pair$sample[3],
                                pair$sample[2],
                                pair$sample[1]
                            )
                            donor_vax <- c(
                                pair$vax[1],
                                pair$vax[1],
                                pair$vax[2],
                                pair$vax[2],
                                pair$vax[3],
                                pair$vax[3]
                            )
                            recipient_vax <- c(
                                pair$vax[2],
                                pair$vax[3],
                                pair$vax[1],
                                pair$vax[3],
                                pair$vax[2],
                                pair$vax[1]
                            )
                            donor_age <- c(
                                pair$age[1],
                                pair$age[1],
                                pair$age[2],
                                pair$age[2],
                                pair$age[3],
                                pair$age[3]
                            )
                            recipient_age <- c(
                                pair$age[2],
                                pair$age[3],
                                pair$age[1],
                                pair$age[3],
                                pair$age[2],
                                pair$age[1]
                            )
                            
                            out <- data.frame(donor, recipient) %>%
                                mutate(
                                    season = seas,
                                    site = sit,
                                    household = house,
                                    strain = pair$strain_type[1],
                                    donor_vax = donor_vax,
                                    recipient_vax = recipient_vax,
                                    donor_age = donor_age,
                                    recipient_age = recipient_age
                                ) %>%
                                select(
                                    season,
                                    site,
                                    household,
                                    donor,
                                    recipient,
                                    strain,
                                    donor_vax,
                                    recipient_vax,
                                    donor_age,
                                    recipient_age
                                )
                            
                        } else if (pair$date[1] == pair$date[2] &
                                   pair$date[2] < pair$date[3] & pair$date[1] < pair$date[3]) {
                            donor <- c(
                                pair$sample[1],
                                pair$sample[2],
                                pair$sample[1],
                                pair$sample[2]
                            )
                            recipient <- c(
                                pair$sample[2],
                                pair$sample[1],
                                pair$sample[3],
                                pair$sample[3]
                            )
                            donor_vax <- c(pair$vax[1],
                                           pair$vax[2],
                                           pair$vax[1],
                                           pair$vax[2])
                            recipient_vax <- c(pair$vax[2],
                                               pair$vax[1],
                                               pair$vax[3],
                                               pair$vax[3])
                            donor_age <- c(pair$age[1],
                                           pair$age[2],
                                           pair$age[1],
                                           pair$age[2])
                            recipient_age <- c(pair$age[2],
                                               pair$age[1],
                                               pair$age[3],
                                               pair$age[3])
                            
                            out <- data.frame(donor, recipient) %>%
                                mutate(
                                    season = seas,
                                    site = sit,
                                    household = house,
                                    strain = pair$strain_type[1],
                                    donor_vax = donor_vax,
                                    recipient_vax = recipient_vax,
                                    donor_age = donor_age,
                                    recipient_age = recipient_age
                                ) %>%
                                select(
                                    season,
                                    site,
                                    household,
                                    donor,
                                    recipient,
                                    strain,
                                    donor_vax,
                                    recipient_vax,
                                    donor_age,
                                    recipient_age
                                )
                            
                        } else {
                            print("New case you didnt think about :(")
                            print(seas)
                            print(as.vector(house))
                            print(nrow(pair))
                        }
                    }
                } else if (nrow(pair) == 4) {
                    if (pair$strain_type[1] != pair$strain_type[2] |
                        pair$strain_type[1] != pair$strain_type[3] |
                        pair$strain_type[1] != pair$strain_type[4] |
                        pair$strain_type[2] != pair$strain_type[3] |
                        pair$strain_type[2] != pair$strain_type[4] |
                        pair$strain_type[3] != pair$strain_type[4] |
                        abs(pair$date[1] - pair$date[2]) > 14 |
                        abs(pair$date[1] - pair$date[3]) > 14 |
                        abs(pair$date[1] - pair$date[4]) > 14 |
                        abs(pair$date[2] - pair$date[3]) > 14 |
                        abs(pair$date[2] - pair$date[4]) > 14 |
                        abs(pair$date[3] - pair$date[4]) > 14) {
                        print("This is likely not a real pair")
                        print(seas)
                        print(as.vector(house))
                        print(nrow(pair))
                    } else if (pair$date[1] == pair$date[2] &
                               pair$date[1] < pair$date[3] &
                               pair$date[1] < pair$date[4]) {
                        donor <- c(
                            pair$sample[1],
                            pair$sample[2],
                            pair$sample[1],
                            pair$sample[2],
                            pair$sample[1],
                            pair$sample[2]
                        )
                        recipient <- c(
                            pair$sample[2],
                            pair$sample[1],
                            pair$sample[3],
                            pair$sample[3],
                            pair$sample[4],
                            pair$sample[4]
                        )
                        donor_vax <- c(
                            pair$vax[1],
                            pair$vax[2],
                            pair$vax[1],
                            pair$vax[2],
                            pair$vax[1],
                            pair$vax[2]
                        )
                        recipient_vax <- c(
                            pair$vax[2],
                            pair$vax[1],
                            pair$vax[3],
                            pair$vax[3],
                            pair$vax[4],
                            pair$vax[4]
                        )
                        donor_age <- c(
                            pair$age[1],
                            pair$age[2],
                            pair$age[1],
                            pair$age[2],
                            pair$age[1],
                            pair$age[2]
                        )
                        recipient_age <- c(
                            pair$age[2],
                            pair$age[1],
                            pair$age[3],
                            pair$age[3],
                            pair$age[4],
                            pair$age[4]
                        )
                        
                        out <- data.frame(donor, recipient) %>%
                            mutate(
                                season = seas,
                                site = sit,
                                household = house,
                                strain = pair$strain_type[1],
                                donor_vax = donor_vax,
                                recipient_vax = recipient_vax,
                                donor_age = donor_age,
                                recipient_age = recipient_age
                            ) %>%
                            select(
                                season,
                                site,
                                household,
                                donor,
                                recipient,
                                strain,
                                donor_vax,
                                recipient_vax,
                                donor_age,
                                recipient_age
                            )
                        
                    } else {
                        print("New case you didnt think about :(")
                        print(seas)
                        print(as.vector(house))
                        print(nrow(pair))
                    }
                } else if (nrow(pair) == 6) {
                    if (nrow(pair %>% select(strain_type) %>% distinct()) > 1 |
                        any(c(0, as.numeric(diff(
                            pair$date
                        ))) > 14)) {
                        stop()
                        print("This is likely not a real pair")
                        print(seas)
                        print(as.vector(house))
                        print(nrow(pair))
                    } else if (pair$date[1] < pair$date[2] &
                               pair$date[1] < pair$date[3] &
                               pair$date[1] < pair$date[4] &
                               pair$date[1] < pair$date[5] & pair$date[6]) {
                        donor <- rep(pair$date[1], 5)
                        recipient <- c(
                            pair$sample[2],
                            pair$sample[3],
                            pair$sample[4],
                            pair$sample[5],
                            pair$sample[6]
                        )
                        
                        out <- data.frame(donor, recipient) %>%
                            mutate(
                                season = seas,
                                site = sit,
                                household = house,
                                strain = pair$strain_type[1]
                            ) %>%
                            select(season,
                                   site,
                                   household,
                                   donor,
                                   recipient,
                                   strain)
                        print("Apparently this branch is used now????????")
                    } else {
                        print("New case you didnt think about :(")
                        print(seas)
                        print(as.vector(house))
                        print(nrow(pair))
                    }
                } else {
                    print("New case you didnt think about :(")
                    print(seas)
                    print(as.vector(house))
                    print(nrow(pair))
                }
                
                list[[n]] <- out
                n <- n + 1
            }
        }
    }
    
    final_pairs <- bind_rows(list) %>%
        mutate(pair_id = row_number())
    return(final_pairs)
}