generate_log_likelihood_function_approx <- function(donor_freqs_observed,
                                                    recipient_freqs_observed,
                                                    Nb_min,
                                                    Nb_max,
                                                    var_calling_threshold,
                                                    confidence_level)
{
    num_NB_values <- Nb_max - Nb_min + 1
    likelihood_matrix <- matrix(0, n_variants, num_NB_values)#This matrix stores the likelihood values for each variant frequency and bottleneck size.
    log_likelihood_matrix <- matrix(0, n_variants, num_NB_values)#This matrix stores the log likelihood values for each variant frequency and bottleneck size.  We can sum the log likelihoods of all the variant alleles to get the total log likelihood.
    log_likelihood_function <- matrix(0, Nb_max)
    
    
    for (i in 1:n_variants) {
        for (j in 1:num_NB_values) {
            Nb_val <- (j - 1 + Nb_min)
            nu_donor <- donor_freqs_observed[i, 1]
            nu_recipient <- recipient_freqs_observed[i, 1]
            if (recipient_freqs_observed[i, 1] >= var_calling_threshold)
            {
                # implement variant calling threshold
                for (k in 0:Nb_val) {
                    likelihood_matrix[i, j] <- likelihood_matrix[i, j] +
                        (
                            dbeta(nu_recipient, k, (Nb_val - k)) * dbinom(k, size = Nb_val, prob = nu_donor)
                        )
                }
                log_likelihood_matrix[i, j] = log(likelihood_matrix[i, j])
            }
            if (recipient_freqs_observed[i, 1] < var_calling_threshold)
            {
                # implement variant calling threshold
                likelihood_matrix[i, j] = 0
                log_likelihood_matrix[i, j] = 0
                for (k in 0:Nb_val) {
                    likelihood_matrix[i, j] <- likelihood_matrix[i, j] +
                        (
                            pbeta(var_calling_threshold, k, (Nb_val - k)) * dbinom(k, size = Nb_val, prob = nu_donor)
                        )
                }
                log_likelihood_matrix[i, j] = log(likelihood_matrix[i, j])
            }
            # Now we sum over log likelihoods of the variants at different loci to get the total log likelihood for each value of Nb
            log_likelihood_function[Nb_val] <- log_likelihood_function[Nb_val] + log_likelihood_matrix[i, j]
            # Shifting entries of log_likelihood function to ensure plot begins at proper point on x axis
        }
    }
    
    
    return(log_likelihood_function)
    
}