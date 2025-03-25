createClonalSummary <- function(data, variable, level) {
  out <- data %>%
    filter(.data[[variable]] == level) %>%
    group_by(clonal_diff) %>%
    tally() %>%
    dplyr::rename(clonalMu = clonal_diff,
                  freq = n) %>%
    mutate(clonalMu = as.character(clonalMu),
           clonalMu = as.numeric(clonalMu))

  t <- data.frame(clonalMu = 0:max(mutation_matrix$clonalMu))

  out <- out %>%
    full_join(t, by = "clonalMu") %>%
    arrange(clonalMu) %>%
    mutate(freq = ifelse(is.na(freq), 0, freq),
           level = level)

  return(out)
}
