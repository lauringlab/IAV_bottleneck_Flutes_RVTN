createClonalDataByCoFactors <- function(clonal_dist_with_meta = clonal_dist_with_meta,
                                        saveForPlotting = FALSE) {
  clonal_dist_with_meta2 <- clonal_dist_with_meta %>%
    mutate(
      subtype = str_extract(strain, "H\\d+N\\d+"),
      season2 = case_when(
        season == 17 ~ "17/18",
        season == 18 ~ "18/19",
        season == 19 ~ "19/20",
        season == 21 ~ "21/22"
      )
    ) %>%
    mutate(
      age_cat = case_when(
        donor_age < 18 & recipient_age < 18 ~ "Child-to-child",
        donor_age < 18 & recipient_age >= 18 ~ "Child-to-adult",
        donor_age >= 18 & recipient_age >= 18 ~ "Adult-to-adult",
        donor_age >= 18 & recipient_age < 18 ~ "Adult-to-child"
      ),
      vax_cat = case_when(
        donor_vax == 0 & recipient_vax == 0 ~ "Neither",
        donor_vax == 0 &
          recipient_vax == 1 ~ "Recipient only",
        donor_vax == 1 & recipient_vax == 0 ~ "Donor only",
        donor_vax == 1 & recipient_vax == 1 ~ "Both"
      ),
      sex_cat = case_when(
        donor_sex == 1 & recipient_sex == 1 ~ "Both Female",
        donor_sex == 1 & recipient_sex == 2 ~ "Female-to-male",
        donor_sex == 2 & recipient_sex == 1 ~ "Male-to-female",
        donor_sex == 2 & recipient_sex == 2 ~ "Both male"
      )
    )
  
  mutation_matrix_17 <- createClonalSummary(clonal_dist_with_meta2, "season2", "17/18")
  mutation_matrix_18 <- createClonalSummary(clonal_dist_with_meta2, "season2", "18/19")
  mutation_matrix_19 <- createClonalSummary(clonal_dist_with_meta2, "season2", "19/20")
  mutation_matrix_21 <- createClonalSummary(clonal_dist_with_meta2, "season2", "21/22")
  
  mutation_matrix_H1N1 <- createClonalSummary(clonal_dist_with_meta2, "subtype", "H1N1")
  mutation_matrix_H3N2 <- createClonalSummary(clonal_dist_with_meta2, "subtype", "H3N2")
  
  mutation_matrix_adultToAdult <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Adult-to-adult")
  mutation_matrix_adultToChild <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Adult-to-child")
  mutation_matrix_childToAdult <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Child-to-adult")
  mutation_matrix_childToChild <- createClonalSummary(clonal_dist_with_meta2, "age_cat", "Child-to-child")
  
  mutation_matrix_both <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Both")
  mutation_matrix_donorOnly <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Donor only")
  mutation_matrix_neither <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Neither")
  mutation_matrix_recipientOnly <- createClonalSummary(clonal_dist_with_meta2, "vax_cat", "Recipient only")
  
  mutation_matrix_bothFemale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Both Female")
  mutation_matrix_bothMale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Both male")
  mutation_matrix_femaleToMale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Female-to-male")
  mutation_matrix_maleToFemale <- createClonalSummary(clonal_dist_with_meta2, "sex_cat", "Male-to-female")
  
  clonal_mut_all <- list()
  
  clonal_mut_all[[1]] <- mutation_matrix_17
  clonal_mut_all[[2]] <- mutation_matrix_18
  clonal_mut_all[[3]] <- mutation_matrix_19
  clonal_mut_all[[4]] <- mutation_matrix_21
  
  clonal_mut_all[[5]] <- mutation_matrix_H1N1
  clonal_mut_all[[6]] <- mutation_matrix_H3N2
  
  clonal_mut_all[[7]] <- mutation_matrix_adultToAdult
  clonal_mut_all[[8]] <- mutation_matrix_adultToChild
  clonal_mut_all[[9]] <- mutation_matrix_childToAdult
  clonal_mut_all[[10]] <- mutation_matrix_childToChild
  
  clonal_mut_all[[11]] <- mutation_matrix_both
  clonal_mut_all[[12]] <- mutation_matrix_donorOnly
  clonal_mut_all[[13]] <- mutation_matrix_neither
  clonal_mut_all[[14]] <- mutation_matrix_recipientOnly
  
  clonal_mut_all[[15]] <- mutation_matrix_bothFemale
  clonal_mut_all[[16]] <- mutation_matrix_bothMale
  clonal_mut_all[[17]] <- mutation_matrix_femaleToMale
  clonal_mut_all[[18]] <- mutation_matrix_maleToFemale
  
  clonal_mut_all[[19]] <- mutation_matrix %>%
    mutate(level = "Overall")
  
  clonal_mut_all <- bind_rows(clonal_mut_all)
  
  if (saveForPlotting == TRUE) {
    write.csv(clonal_mut_all, file = '/Users/katykrupinsky/git/FluTES_bottleneck/04 - Output/clonal_dist_by_factor.csv')
    
    num_in <- clonal_mut_all %>%
      group_by(level) %>%
      summarise(num = sum(freq))
    
    write.csv(num_in, file = '/Users/katykrupinsky/git/FluTES_bottleneck/04 - Output/clonal_num_samples.csv')
  }
  
  return(clonal_mut_all)
}
