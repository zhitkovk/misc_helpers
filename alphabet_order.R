# Ordering each row of the dataframe column in alphabetical order
tl <- strsplit(tds_df$comb, "")

tl <- lapply(1:length(tl), function(x) str_sort(tl[[x]]))

tl <- sapply(1:length(tl), function(x) Reduce(paste0, tl[[x]]))