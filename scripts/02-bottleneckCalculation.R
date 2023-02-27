## Title: Bottleneck Calculations
## Date Last Modified: 02/27/23
## Description: This code was pulled from the github page for the Koelle lab 
## bottleneck calculation paper (https://github.com/koellelab/betabinomial_bottleneck).
## Additions were made to put the calculation into a loop and output the
## data frame as separate files.
##
## -----------------------------------------------------------------------------

# 0. Load necessary libraries and set up data frames ---------------------------
library(tidyverse)
library(argparse)

pairs <- seq(1, 21)
all_zero <- data.frame(pair = 1:length(pairs),
                       bool = NA,
                       n_variants = 0)
confidence <- data.frame(pair = 1:length(pairs),
                         max_LL = 000,
                         upper_CI = 000,
                         lower_CI = 000)

for (a in pairs){

    pathname <- paste("./processedData/",
                                  paste(paste("pair", a, sep = "_"), ".txt",
                                        sep = ""), sep = "")
    filename <- paste("./bottleneckOutput/",
                      paste(paste("output_pair", a, sep = "_"), ".txt",
                            sep = ""), sep = "")
    
    ## 1. Handle command line arguments ----------------------------------------
    parser <- ArgumentParser()
    
    parser$add_argument("--file", type="character", default = pathname ,
                        help="file containing variant frequencies")
    parser$add_argument("--plot_bool", type="logical", default= FALSE,
                        help="determines whether pdf plot approx_plot.pdf is produced or not")
    parser$add_argument("--var_calling_threshold", type="double", default= 0.005,
                        help="variant calling threshold")
    parser$add_argument("--Nb_min", type="integer", default= 1,
                        help="Minimum bottleneck value considered")
    parser$add_argument("--Nb_max", type="integer", default= 200,
                        help="Maximum bottleneck value considered")
    parser$add_argument("--Nb_increment", type="integer", default= 1,
                        help="increment between Nb values considered, i.e., all values considered will be multiples of Nb_increment that fall between Nb_min and Nb_max")
    parser$add_argument("--confidence_level", type="double", default= .95,
                        help="Confidence level (determines bounds of confidence interval)")
    args <- parser$parse_args()
    
    ## 2. Create necessary variables and data frames ---------------------------
    
    # determines whether a plot of log likelihood vs bottleneck size is produced
    plot_bool  <- args$plot_bool
    
    # variant calling threshold for frequency in recipient
    var_calling_threshold  <- args$var_calling_threshold
    
    # minimum bottleneck size we consider
    Nb_min <- args$Nb_min  
    if(Nb_min < 1){
        Nb_min = 1
    }     # preventing errors with Nb_min at 0 or lower 
    
    # maximum bottleneck size we consider
    Nb_max <-  args$Nb_max
    
    Nb_increment <- args$Nb_increment
    
    # determines width of confidence interval
    confidence_level <- args$confidence_level
    
    # table of SNP frequencies in donor and recipient
    donor_and_recip_freqs_observed <- read.table(args$file)
    
    # number of rows in raw table
    original_row_count <- nrow(donor_and_recip_freqs_observed)
    
    donor_and_recip_freqs_observed <- 
        subset(donor_and_recip_freqs_observed,
               donor_and_recip_freqs_observed[, 1] >= var_calling_threshold)
    
    donor_and_recip_freqs_observed[, 1] <- 
        as.numeric(donor_and_recip_freqs_observed[, 1])
    
    donor_and_recip_freqs_observed[, 2] <- 
        as.numeric(donor_and_recip_freqs_observed[, 2])
    
    donor_and_recip_freqs_observed <- 
        subset(donor_and_recip_freqs_observed,
               donor_and_recip_freqs_observed[, 1] <= (1-var_calling_threshold))
    
    # number of rows in filtered table
    new_row_count <- nrow(donor_and_recip_freqs_observed)
    
    if(new_row_count != original_row_count){
        print("WARNING:  Rows of the input file with donor frequency less than variant calling threshold have been removed during analysis. ")
    }
    
    if(new_row_count == 0){
        print("!!!WARNING!!!: Observed frequency table does not fall within varient calling threshold. All output will be zeros")
    }
    
    # number of variants 
    n_variants <- nrow(donor_and_recip_freqs_observed)
    
    freqs_tibble <- tibble(donor_freqs = donor_and_recip_freqs_observed[, 1],
                           recip_freqs = donor_and_recip_freqs_observed[, 2] )
    
    # 3. Implement the beta binomial algorithm ---------------------------------
    
    if (new_row_count == 0){
        all_zero[a, 2] <- TRUE
        all_zero[a, 3] <- n_variants
        confidence[a, 2] <- NA
        confidence[a, 3] <- NA
        confidence[a, 4] <- NA
    } else {
        all_zero[a, 2] <- FALSE
        all_zero[a, 3] <- n_variants
    
        ## Get Log-Likelihood for every donor recipient SNP frequency pair
        Log_Beta_Binom <- function(nu_donor, nu_recipient, NB_SIZE) {
            # used for recipient frequencies above calling threshold
            LL_val_above <- 0
            
            # used for recipient frequencies below calling threshold
            LL_val_below <- 0
        
            nu_donor <- if_else(nu_recipient <= 1 - var_calling_threshold,
                                nu_donor,  1-nu_donor)
            
            nu_recipient <- if_else(nu_recipient <= 1 - var_calling_threshold,
                                    nu_recipient,  1-nu_recipient)
            for(k in 0:NB_SIZE){
                LL_val_above <-  LL_val_above +
                    dbeta(nu_recipient, k, (NB_SIZE - k)) *
                    dbinom(k, size = NB_SIZE, prob = nu_donor)
                LL_val_below <- LL_val_below +
                    pbeta(var_calling_threshold, k, (NB_SIZE - k)) *
                    dbinom(k,size = NB_SIZE, prob = nu_donor)
                }
            LL_val <- if_else(nu_recipient >= var_calling_threshold,
                              LL_val_above, LL_val_below)
        
        # We use LL_val_above above the calling threshold, and LL_val_below
        # below the calling threshold
            
            # convert likelihood to log likelihood
            LL_val <- log(LL_val)
            return(LL_val)
        }
        
        # This function sums over all SNP frequencies in the donor and recipient
        LL_func_approx <- function(Nb_size){
            Total_LL <- 0
            LL_array <- Log_Beta_Binom(freqs_tibble$donor_freqs,
                                       freqs_tibble$recip_freqs, Nb_size)  
            Total_LL <- sum(LL_array)
            return(Total_LL)
        }
        
        bottleneck_values_vector <- c()
        for (b in Nb_min:Nb_max) { 
            if(b%%Nb_increment == 0) {
                bottleneck_values_vector <- c(bottleneck_values_vector, b)
            }
        }
        
        LL_tibble <- tibble(bottleneck_size = bottleneck_values_vector,
                            Log_Likelihood = 0 * bottleneck_values_vector) 
        
        for(c in 1:nrow(LL_tibble) ){
            LL_tibble$Log_Likelihood[c] <-
                LL_func_approx(LL_tibble$bottleneck_size[c])
        }
        
    # 4. Find the MLE and the associated confidence interval -------------------
        
        # Maximum value of log likelihood
        Max_LL <- max(LL_tibble$Log_Likelihood)
        
        # bottleneck size at which max likelihood occurs
        Max_LL_bottleneck_index <- 
            which(LL_tibble$Log_Likelihood == max(LL_tibble$Log_Likelihood))
        
        Max_LL_bottleneck <- bottleneck_values_vector[Max_LL_bottleneck_index]
        
        # necessary ratio of likelihoods set by confidence level
        likelihood_ratio <- qchisq(confidence_level, df = 1)
        
        ci_tibble <- filter(LL_tibble,
                            2 * (Max_LL - Log_Likelihood) <= likelihood_ratio) 
        
        #-1 # lower bound of confidence interval
        lower_CI_bottleneck <- min(ci_tibble$bottleneck_size)
        
        #+1# upper bound of confidence interval
        upper_CI_bottleneck <- max(ci_tibble$bottleneck_size)
        
        #if ci_tibble is empty
        if (length(ci_tibble$Log_Likelihood) == 0) {
            lower_CI_bottleneck <- min(Max_LL_bottleneck) 
            upper_CI_bottleneck <- max(Max_LL_bottleneck)
        }
        if(max(Max_LL_bottleneck) == max(bottleneck_values_vector)){
            upper_CI_bottleneck <- max(bottleneck_values_vector)
            print("Peak bottleneck value for MLE is at Nb_max (or largest possible value given Nb_increment)!  Try raising Nb_max for better bottleneck estimate")
        }
        if(min(Max_LL_bottleneck) == min(bottleneck_values_vector)){
            lower_CI_bottleneck <- min(bottleneck_values_vector)
            print("Minimum bottleneck value for MLE is at Nb_min (or smallest possible value given Nb_increment)!  Try lowering Nb_min for better bottleneck estimate")
            }
        
        confidence[a, 2] <- Max_LL_bottleneck
        confidence[a, 3] <- upper_CI_bottleneck
        confidence[a, 4] <- lower_CI_bottleneck
        
    ## 5. Write output to table at previously defined file name -----------------
        write.table(LL_tibble,
                    file = filename,
                    sep = "\t",
                    row.names = FALSE, col.names = FALSE)
    }
}

## 6. Write housekeeping output to permanent files -----------------------------
write.table(all_zero, 
            file = "./bottleneckOutput/all_zero.txt",
            sep = "\t",
            row.names = FALSE)
write.table(confidence, 
            file = "./bottleneckOutput/confidence_int.txt",
            sep = "\t",
            row.names = FALSE)

## 7. Clean up environment -----------------------------------------------------
rm(list=setdiff(ls(), c()))
