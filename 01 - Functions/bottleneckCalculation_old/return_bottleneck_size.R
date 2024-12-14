return_bottleneck_size <- function(log_likelihood_function, Nb_min, Nb_max)
{
    max_log_likelihood = which(log_likelihood_function == max(log_likelihood_function))
    
    return(max_log_likelihood)
}