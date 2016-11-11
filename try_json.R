tryJSON <- function(file) {
  d <- try(fromJSON(file), silent = TRUE)
  if (inherits(d, "try-error")) {
    d <- "Oops"
  } else {
    d <- fromJSON(file)
  }
  return(d)
}