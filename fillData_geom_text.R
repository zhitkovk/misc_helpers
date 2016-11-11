fillData <- function(data, categorical = FALSE, init_count = NULL, ...) {
  # TODO: Probably needs two groupings as input for categorical case
  # Args: data, variable to group_by and variable with initial counts if 
  # they were done
  if (categorical == TRUE) {
    data <- data %>%
      group_by_(...) %>%
      summarise(count = n()) %>%
      mutate(freqs = count/sum(count),
             position_abs = cumsum(count) - 0.5 * count,
             position_rel = cumsum(freqs) - 0.5 * freqs)
  } else {
    dots <- list(interp(~ (variable/sum(variable)), 
                        variable = as.name(init_count)),
                 interp(~ (cumsum(variable/sum(variable)) - 0.5 * (variable/sum(variable))), 
                        variable = as.name(init_count)),
                 interp(~ (cumsum(variable) - 0.5 * variable), 
                        variable = as.name(init_count)))
    data <- data %>%
      group_by_(...) %>%
      mutate_(.dots = setNames(dots, c("freqs", "position_rel", "position_abs")))
  }
  return(data)
}
