# Check whether variable has label attribute
attrCheck <- function(vect) {
  vect_attr <- is.null(attributes(vect)$labels)
  return(vect_attr)
}

# Frequencies
freq <- function(vect) {
  vect <- vect/sum(vect, na.rm = TRUE)
  return(vect)
}