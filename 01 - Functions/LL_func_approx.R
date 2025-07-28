# This function sums over all SNP frequencies in the donor and recipient

LL_func_approx <- function(Nb_size){
    Total_LL <- 0
    LL_array <- Log_Beta_Binom(nu_donor = freqs_tibble$donor_freqs, nu_recipient = freqs_tibble$recip_freqs, NB_SIZE = Nb_size)  
    Total_LL <- sum(LL_array)
    return(Total_LL)
}