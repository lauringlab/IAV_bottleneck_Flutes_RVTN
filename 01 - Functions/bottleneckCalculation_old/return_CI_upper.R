return_CI_upper <- function(log_likelihood_function, Nb_min, Nb_max)
    ## returns upper bound of confidence interval
{
    max_log_likelihood = which(log_likelihood_function == max(log_likelihood_function))  ## This is the point on the x-axis (bottleneck size) at which log likelihood is maximized
    max_val =  max(log_likelihood_function)  ## This is the maximum value of the log likelihood function, found when the index is our bottleneck estimate
    CI_height = max_val - erfinv(confidence_level) * sqrt(2)  # This value (  height on y axis) determines the confidence intervals using the likelihood ratio test
    
    CI_index_lower = Nb_min
    CI_index_upper = max_log_likelihood
    for (h in 1:Nb_min) {
        if (h < Nb_min)
        {
            log_likelihood_function[h] = NA
        }   #  Removing parameter values less than Nb_min from plot
    }
    ## above loop just enforces our minimum bottleneck cutoff
    for (h in Nb_min:max_log_likelihood) {
        test1 = (log_likelihood_function[CI_index_lower] - CI_height) * (log_likelihood_function[CI_index_lower] - CI_height)
        test2 = (log_likelihood_function[h] - CI_height) * (log_likelihood_function[h] - CI_height)
        if (test2 < test1) {
            CI_index_lower = h
        }
    }
    if ((log_likelihood_function[CI_index_lower] - CI_height) > 0) {
        CI_index_lower = CI_index_lower - 1
    }
    # above loops use likelihood ratio test to find lower confidence interval
    for (h in max_log_likelihood:Nb_max)
    {
        test1 = (log_likelihood_function[CI_index_upper] - CI_height) * (log_likelihood_function[CI_index_upper] - CI_height)
        test2 = (log_likelihood_function[h] - CI_height) * (log_likelihood_function[h] - CI_height)
        if (test2 < test1) {
            CI_index_upper = h
        }
    }
    if ((log_likelihood_function[CI_index_upper] - CI_height) > 0) {
        CI_index_upper = CI_index_upper + 1
    }
    
    return(CI_index_upper)
    
}