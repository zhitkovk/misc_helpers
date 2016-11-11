# Simple barplot ----
barPlotter <- function(data, variable, xlab, title) {
  plot <- ggplot(data = data, aes_string(x = variable)) +
    geom_bar(fill = cgbs_pal[1]) +
    geom_text(stat = "count", aes(label = ..count..), vjust = - 0.7, size = 4.5) +
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) + 
    labs(x = xlab,
         y = "Number of respondents",
         title = title) + 
    theme_bw_mod()
  return(plot)
}

# Dodged barplot ----
dodgedBar <- function(data, variable, var_fill, fill_title, xlab, title) {
  plot <- ggplot(data = data, aes_string(x = variable, fill = var_fill)) +
    geom_bar(position = position_dodge(width = 0.9)) +
    geom_text(stat = "count", 
              aes(label = ..count..), 
              position = position_dodge(width = 0.9),
              vjust = - 0.7, 
              size = 4.5) +
    scale_fill_manual(name = fill_title,
                      values = cgbs_pal) + 
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) + 
    labs(x = xlab,
         y = "Number of respondents",
         title = title) + 
    theme_bw_mod()
  return(plot)
}


# Boxplot ----
boxPlotter <- function(data, var_x, var_y, var_color, xlab, title) {
  plot <- ggplot(data = data,
                 aes_string(x = var_x, y = var_y, color = var_color)) +
    geom_boxplot() +
    scale_color_manual(name = "Who pays for you",
                       values = cb_pal) + 
    scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
    labs(x = xlab,
         y = "USE result [0;100]",
         title = title) + 
    theme_bw_mod()
  return(plot)
}




# Likert plot ----

# Prepare data
fillData <- function(data, ...) {
  # TODO: Probably needs two groupings as input for categorical case
  # Args: data, variable to group_by and variable with initial counts if 
  # they were done
  data <- data %>%
    group_by_(...) %>%
    summarise(count = n()) %>%
    mutate(freqs = freq(count),
           position = cumsum(freqs) - 0.5 * freqs)
  return(data)
}


# Plot itself for 4th Wave
likertPlotter <- function(data, var_x, var_fill, title) {
  likert <- ggplot(data, aes_string(x = var_x , y = "count" , fill = var_fill)) +
    geom_bar(stat = "identity", position = position_fill()) +
    geom_text(aes(y = position, label = ifelse(freqs > 0.02, sprintf("%1.0f%%", 100 * freqs), ""))) +
    scale_x_discrete(labels = str_wrap(scale_uni, width = 10)) +
    scale_fill_manual(name = "Degree of\nagreement:",
                      labels = scale_agree,
                      values = cgbs_pal) +
    labs(x = "Happiness with university choice",
         y = "Fraction of respondents",
         title = paste("Happiness with university choice and", title)) +
    theme_bw_mod() +
    coord_flip()
  return(likert)
}


# Plot for 3rd wave
likertPlotter3 <- function(data, var_x, var_fill, title) {
  likert <- ggplot(data, aes_string(x = var_x , y = "count" , fill = var_fill)) +
    geom_bar(stat = "identity", position = position_fill()) +
    geom_text(aes(y = position, label = ifelse(freqs > 0.02, sprintf("%1.0f%%", 100 * freqs), ""))) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_fill_manual(name = "Степень\nсогласия:",
                      labels = function(x) str_wrap(x, width = 10),
                      values = cgbs_pal) +
    labs(x = "Утверждение",
         y = "Доля респондентов",
         title = title) +
    theme_bw_mod()
  return(likert)
}