# Easy functions -----------------------
#
#
#
index <- function(varlist, df) {
  # Function for subsetting by names 
  # (when you don't have dplyr and don't remember base::subset:))
  #
  # Args:
  #  varlist: vector of variable names
  #  df: dataframe to subset
  # 
  # Returns:
  #  Indices of mentioned variables for further subsetting
  subset <- sapply(1:length(varlist),
                   function(x) which(colnames(df) == varlist[x]))
  return(subset)
}
#
#
#
# Example: leaving three columns
data <- mtcars
subset.data <- data[, index("hp","drat", data)]
#
#
#
# Normalizing variables of interest
normalizer <- function(vect) {
  # Transforms vector to belong to range [0;1]
  #
  # Args:
  #  vect: numeric vector
  #
  # Returns:
  #  Vector that is transfromed to belong to range [0;1]
  vect <- (vect - min(vect))/
    (max(vect) - min(vect))
  return(vect)
}
#
#
#
# Basic text cleaner
textCleaner <- function(text.vec) {
  # Removes puntcuation, control characters and numbers
  # And transforms to lowercase
  #
  # Args:
  #   text.vec: character vector
  #
  # Returns:
  #  Clean lowercase character vector
  text.vec <- gsub("[[:punct:]]", "", text.vec)
  text.vec <- gsub("[[:cntrl:]]", "", text.vec)
  text.vec <- gsub("[[:digit:]]", "", text.vec)
  text.vec <- tolower(text.vec)
}
#
#
#
# Summary statistics for lapply or dplyr summarise_each
sumStat <- function(variable){
  # Calculates some basic summary statistics for a vector
  # Convenient for lapply and dplyr summarise_each. Example below
  #
  # Args:
  #  variable: vector you want to compute summary statistics for
  #
  # Returns:
  #  Named vector with summary statistics
  stats <- c(mean(variable, na.rm = T),
             sd(variable, na.rm = T),
             median(variable, na.rm = T),
             min(variable, na.rm = T),
             max(variable, na.rm = T))
  names(stats) <- c("mean",
                    "sd",
                    "median",
                    "min",
                    "max")
  return(stats)
}
#
# sumStat example with lapply
sum.stats <- lapply(mtcars, sumStat) %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame(row.names = colnames(mtcars))
#
#
#
# Random group_by sample for finding unexpected facts about data
randomGroupBy <- function(data, size) {
  data %>%
    group_by_(sample(colnames(data), size = size))
  print(paste0("Selected features are: ", sample(colnames(data), size = size)))
  return(data)
}
#
#
#
# TODO: 
# several similar dataframes and you need to name columns in each of them
columns <- c("n", "place", "region")

data <- lapply(data, setNames, columns)


# Get all lists from environment into one
objs <- as.list(globalenv())
objs <- objs[sapply(objs, class) == "list"]

# function which checks the duplicated values in the dataframe

# read many files from directory
readRenameSubset2 <- function(data, column.names, key) {
  data <- data[-1,]
  colnames(data) <- column.names
  data$type <- key
  return(data)
}
# TODO: Add regexp for finding hyperlinks
# Generating date sequence -----------------------
# years <- seq.Date(as.Date("2005-01-01"), by = "1 year", length.out = 10)