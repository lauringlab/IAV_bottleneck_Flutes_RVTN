determineTransmissionPairs_clonal <- function(df) {
  # 1. Setup step --------------------------------------------------------------
  household <- df %>%
    dplyr::select(hhid) %>%
    distinct() %>%
    as_vector()
  
  list <- list()
  
  for (n in 1:length(household)) {
    house <- household[n]
    pair <- df %>%
      filter(hhid == house) %>%
      dplyr::select(sample,
             site,
             season,
             age,
             sex,
             vax,
             onset_date,
             collection_date,
             strain) %>%
      distinct() %>%
      arrange(onset_date)
    
    # 2. Pull out matrix indexes -------------------------------------------------
    if (nrow(pair) < 2) {
      print("There aren't enough people in this household...")
    } else if (nrow(pair) == 2) {
      if (pair$strain[1] != pair$strain[2] |
          abs(pair$onset_date[1] - pair$onset_date[2]) > 14) {
        print("This is likely not a real pair")
      }
      else {
        if (pair$onset_date[1] != pair$onset_date[2]) {
          donor_index <- 1
          recipient_index <- 2
        } else {
          donor_index <- c(1)
          recipient_index <- c(2)
        }
      }
    } else if (nrow(pair) == 3) {
      if (pair$strain[1] != pair$strain[2] |
          pair$strain[1] != pair$strain[3] |
          pair$strain[2] != pair$strain[3] |
          abs(pair$onset_date[1] - pair$onset_date[2]) > 14 |
          abs(pair$onset_date[1] - pair$onset_date[3]) > 14 |
          abs(pair$onset_date[3] - pair$onset_date[2]) > 14) {
        print("This is likely not a real pair")
      } else {
        if (pair$onset_date[1] < pair$onset_date[2] &
            pair$onset_date[1] < pair$onset_date[3]) {
          donor_index <- c(1, 1)
          recipient_index <- c(2, 3)
        } else if (pair$onset_date[1] == pair$onset_date[2] &
                   pair$onset_date[2] == pair$onset_date[3]) {
          donor_index <- c(1, 1, 2)
          recipient_index <- c(2, 3, 3)
        } else if (pair$onset_date[1] == pair$onset_date[2] &
                   pair$onset_date[2] < pair$onset_date[3] &
                   pair$onset_date[1] < pair$onset_date[3]) {
          donor_index <- c(1, 1, 2)
          recipient_index <- c(2, 3, 3)
        } else {
          print("New case you didnt think about :(")
          print(pair)
        }
      }
    } else if (nrow(pair) == 4) {
      if (pair$strain[1] != pair$strain[2] |
          pair$strain[1] != pair$strain[3] |
          pair$strain[1] != pair$strain[4] |
          pair$strain[2] != pair$strain[3] |
          pair$strain[2] != pair$strain[4] |
          pair$strain[3] != pair$strain[4] |
          abs(pair$onset_date[1] - pair$onset_date[2]) > 14 |
          abs(pair$onset_date[1] - pair$onset_date[3]) > 14 |
          abs(pair$onset_date[1] - pair$onset_date[4]) > 14 |
          abs(pair$onset_date[2] - pair$onset_date[3]) > 14 |
          abs(pair$onset_date[2] - pair$onset_date[4]) > 14 |
          abs(pair$onset_date[3] - pair$onset_date[4]) > 14) {
        print("This is likely not a real pair")
      } else if (pair$onset_date[1] == pair$onset_date[2] &
                 pair$onset_date[1] < pair$onset_date[3] &
                 pair$onset_date[1] < pair$onset_date[4]) {
        donor_index <- c(1, 1, 2, 1, 2)
        recipient_index <- c(2, 3, 3, 4, 4)
      } else if (pair$onset_date[1] < pair$onset_date[2] &
                 pair$onset_date[1] < pair$onset_date[3] &
                 pair$onset_date[1] < pair$onset_date[4]) {
        donor_index <- c(1, 1, 1)
        recipient_index <- c(2, 3, 4)
      } else {
        print("New case you didnt think about :(")
        print(pair)
      }
    } else if (nrow(pair) == 5) {
      if (nrow(pair %>% dplyr::select(strain) %>% distinct()) > 1 |
          any(c(0, as.numeric(diff(
            pair$onset_date
          ))) > 14)) {
        print("This is likely not a real pair")
      } else if (pair$onset_date[1] < pair$onset_date[2] &
                 pair$onset_date[2] < pair$onset_date[3] &
                 pair$onset_date[2] < pair$onset_date[4] &
                 pair$onset_date[1] < pair$onset_date[5]) {
        donor_index <- c(1, 1, 1, 1)
        recipient_index <- c(2, 3, 4, 5)
      } else {
        print("New case you didnt think about :(")
        print(pair)
      }
    } else if (nrow(pair) == 6) {
      if (nrow(pair %>% dplyr::select(strain) %>% distinct()) > 1 |
          any(c(0, as.numeric(diff(
            pair$onset_date
          ))) > 14)) {
        print("This is likely not a real pair")
      } else if (pair$onset_date[1] < pair$onset_date[2] &
                 pair$onset_date[1] < pair$onset_date[3] &
                 pair$onset_date[1] < pair$onset_date[4] &
                 pair$onset_date[1] < pair$onset_date[5] &
                 pair$onset_date[1] < pair$onset_date[6]) {
        donor_index <- c(1, 1, 1, 1, 1)
        recipient_index <- c(2, 3, 4, 5, 6)
      } else {
        print("New case you didnt think about :(")
        print(pair)
      }
    } else if (nrow(pair) == 7) {
      if (nrow(pair %>% dplyr::select(strain) %>% distinct()) > 1 |
          any(c(0, as.numeric(diff(
            pair$onset_date
          ))) > 14)) {
        print("This is likely not a real pair")
      } else if (pair$onset_date[1] < pair$onset_date[2] &
                 pair$onset_date[1] < pair$onset_date[3] &
                 pair$onset_date[1] < pair$onset_date[4] &
                 pair$onset_date[1] < pair$onset_date[5] &
                 pair$onset_date[1] < pair$onset_date[6] &
                 pair$onset_date[1] < pair$onset_date[7]) {
        donor_index <- c(1, 1, 1, 1, 1, 1)
        recipient_index <- c(2, 3, 4, 5, 6, 7)
      } else {
        print("New case you didnt think about :(")
        print(pair)
      }
    } else {
      print("New case you didnt think about :(")
      print(pair)
    }
    
    # 3. Create our output -----------------------------------------------------
    out <- tibble(
      season = pair$season[donor_index],
      site = pair$site[donor_index],
      hhid = house,
      donor_id = pair$sample[donor_index],
      recipient_id = pair$sample[recipient_index],
      strain = pair$strain[donor_index],
      donor_vax = pair$vax[donor_index],
      recipient_vax = pair$vax[recipient_index],
      donor_age = pair$age[donor_index],
      recipient_age = pair$age[recipient_index],
      donor_sex = pair$sex[donor_index],
      recipient_sex = pair$sex[recipient_index],
      donor_onset = pair$onset_date[donor_index],
      recipient_onset = pair$onset_date[recipient_index],
      donor_collection = pair$collection_date[donor_index],
      recipient_collection = pair$collection_date[recipient_index]
    )
    
    list[[n]] <- out
  }
  
  final_pairs <- bind_rows(list) %>%
    arrange(season, donor_onset, site) %>%
    mutate(pair_id = row_number())
  return(final_pairs)
}
