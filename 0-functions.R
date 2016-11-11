numericCutter <- function(vect, step = 0.2) {
  # Wrapper for cut function
  # Takes numeric vector as input
  # Returns factor based on intervals
  vect <- round(vect, 0)
  vect <- cut(vect,
              breaks = unique(round((quantile(vect, 
                                       probs = seq(0, 1, step),
                                       names = FALSE,
                                       na.rm = TRUE)), -2)),
              dig.lab = 10,
              ordered_result = TRUE,
              include.lowest = TRUE,
              right = FALSE)
  return(vect)
}


