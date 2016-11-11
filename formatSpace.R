# Add spaces to large numbers. Useful in ggplot text

formatSpace <- function(vect) {
  vect <- format(vect, big.mark = " ", scientific = FALSE, trim = TRUE)
  return(vect)
}
