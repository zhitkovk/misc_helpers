# Example of web scraping a rather simple html data. The source contains data
# on number of students in Russian universities

# Libraries
library("dplyr")
library("reshape2")
library("rvest")

# Read html into R
url <- "http://www.gks.ru/free_doc/new_site/population/obraz/vp-obr1.htm"
url <- read_html(url)

data <- 
  url %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  as.data.frame()

# Delete columns and rows with information of no interest
file <- data %>%
  select(-c(1,9)) %>%
  slice(-(24:nrow(data))) %>%
  slice(-(1:3))

colnames(file) <- c("year", "n.org", "n.stud",
                    "n.daily", "n.evening", "n.extern",
                    "n.extern.old")

# Text cleaning and splitting year variable
file[file == "-"] <- NA
file$year <- gsub("\\r\\n\\(на 15 декабря\\)", "", file$year)

# Split year variable (beginning and end of the year)
file[, 8:9] <- colsplit(string=file$year, 
                        pattern="/", 
                        names = c("year.beg", "year.end"))

# Bruteforce way to add zero character for years after 2000
file$year.end <- ifelse(nchar(file$year.end) < 2, 
                        paste0("0", as.character(file$year.end)),
                        as.character(file$year.end))

# Bruteforce way of creating four digit year continued
file$year.end <- ifelse(file$year.end > 20,
                paste0("19", as.character(file$year.end)), 
                paste0("20", as.character(file$year.end)))

# Coercing character to numeric
file[2:ncol(file)] <- lapply(file[2:ncol(file)], 
                             gsub, pattern = ",", 
                             replacement = ".")
file[2:ncol(file)] <- lapply(file[2:ncol(file)], as.integer)