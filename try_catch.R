library("gdata")


# Read data
read_xls_wrapper <- function(file_link, key_one, key_two) {
  # Read data, subset empty rows
  # Add key_one -- region
  # Add key_two -- state/ private
  d <- read.xls(URLencode(file_link), sheet = 6, skip = 5)
  d <- d[1:22, ]
  # colnames(d) <- cols
  d$region <- key_one
  d$owner <- key_two
  return(d)
}

try_wrapper <- function(file_link, key_one, key_two) {
  
  out <- tryCatch(
    {
      read_xls_wrapper(file_link, key_one, key_two)
    },
    
    error = function(cond) {
      message(paste("URL is empty:", file_link))
      return(NA)
    },
    
    warning = function(cond) {
      return(NA)
    }
  )
  return(out)
}
  

