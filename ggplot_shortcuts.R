# ggplot templates and functions
#
#
#
histPlot <- function(data, variable, title, xlab) {
  # Plots histograms. Convenient for apply family of functions.
  # Nice to use it with it in exploratory analysis
  #
  # Args:
  #  data: your dataframe
  #  variable: variable to plot, character vector required
  #  title: title of the plot, character vector required
  #  xlab: x axis name
  #
  # Returns:
  #  ggplot histogram (minimalistic theme)
  plot = ggplot(data = data, aes_string(x = variable)) +
    geom_bar(stat = "bin") +
    labs(title = title,
         x = xlab) +
    theme_bw() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 14))
  return(plot)
}
#
# histPlot example with lapply
d <- lapply(1:ncol(mtcars), function(x) histPlot(mtcars,
                                                 colnames(mtcars)[x], 
                                                 colnames(mtcars)[x], 
                                                 colnames(mtcars)[x]))