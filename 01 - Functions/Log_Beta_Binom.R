# This function gives Log Likelihood for every donor recipient SNP frequency pair

Log_Beta_Binom <- function(nu_donor, nu_recipient, NB_SIZE) {
    LL_val_above <- 0
    LL_val_below <- 0
    
    ## KATY NOTE: I am not sure why we have to do this, but it seems fine?
    nu_donor <- if_else(nu_recipient <= 1 - var_calling_threshold ,
                        nu_donor,
                        1 - nu_donor)
    
    nu_recipient <- if_else(nu_recipient <= 1 - var_calling_threshold ,
                            nu_recipient,
                            1 - nu_recipient)
    
    for (k in 0:NB_SIZE) {
        LL_val_above <-  LL_val_above +  dbeta(nu_recipient, k, (NB_SIZE - k)) *
            dbinom(k, size = NB_SIZE, prob = nu_donor)
        LL_val_below <- LL_val_below +   pbeta(var_calling_threshold, k, (NB_SIZE - k)) *
            dbinom(k, size = NB_SIZE, prob = nu_donor)
    }
    
    # Gives us a likelihood value for each of the sites that made it past filtering in a pair
    LL_val <- if_else(nu_recipient >= var_calling_threshold ,
                      LL_val_above,
                      LL_val_below)
    
    # We use LL_val_above above the calling threshold, and LL_val_below below the calling threshold
    
    LL_val <- log(LL_val) # convert likelihood to log likelihood
    return(LL_val)
}