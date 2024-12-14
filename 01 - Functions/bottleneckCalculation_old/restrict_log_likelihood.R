restrict_log_likelihood <- function(log_likelihood_function, Nb_min, Nb_max)
    # restricts log likelihood to the interval of interst
{
    for (h in 1:(Nb_min)) {
        if (h < Nb_min)
        {
            log_likelihood_function[h] = -999999999
        }        # kludge for ensuring that these values less than Nb_min don't interfere with our search for the max of log likelihood in the interval of Nb_min to Nb_max
    }
    
    return(log_likelihood_function)
    #print(erfinv(percent_confidence_interval)*sqrt(2))
}