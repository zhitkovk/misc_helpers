library("dplyr")
library("ggplot2")
d <- mtcars


d$cyl <- factor(d$cyl)
d$am <- factor(d$am)

fillData <- function(data, ...) {
  # Function counts frequencies based on supplied groupping variables
  # and provides position variable for adding them to bar chart.
  # 
  # Args:
  #  data: your table with data
  #  ...: variables you group your dataset by. 
  #       You need 2: one for x axis and one for bar filling
  #
  # Returns:
  #  Dataframe with counted group frequencies and 
  #  position variable for plotting them
  data <- data %>%
    group_by_(...) %>%
    summarise(count = n()) %>%
    mutate(freqs = count/sum(count),
           position_rel = cumsum(freqs) - 0.5 * freqs,
           position_abs = cumsum(count) - 0.5 * count)
  return(data)
}


df <- fillData(d, "am", "cyl")

p0 <- ggplot(data = d, aes(x = am)) +
  geom_bar(aes(fill = cyl), position = position_fill()) +
  theme_visible()

p0


p1 <- ggplot(data = df, aes(x = am, y = freqs, fill = cyl)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = position, label = round(freqs, 2))) +
  theme_visible()

p1

# for total labels
dft <- df %>%
  group_by(am) %>%
  summarise(tot_sum = sum(count))

p2 <- ggplot(data = df, aes(x = am, y=count)) +
  geom_bar(stat="identity", aes(fill = cyl)) +
  geom_text(aes(y = position_abs, label = paste0(round(100*freqs, 2), "%"))) +
  geom_text(aes(y = tot_sum, x = am, fill = NULL, label = paste0("Всего: ", tot_sum)), data = dft, vjust = -0.4) +
  labs(title = "WOW",
       x = "pow",
       y = "how") +
  theme_visible()

p2


