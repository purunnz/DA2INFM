#' Plot Influence Measures
#'
#' This function plots the selected influence measures for a given linear model.
#' It combines Cook's Distance, DFFITS, and Hadi's Influence Measure in one plot.
#'
#' @param data A data frame or matrix containing the data used in the model.
#' @param model A linear model object of class 'lm'.
#' @param measures A character vector specifying which influence measures to plot.
#'        Default is c("cooks", "dffits", "hadi").
#'
#' @return A plot object displaying the selected influence measures.
#' @export
plot_influence_measures <- function(data, model, measures = c("cooks", "dffits", "hadi")) {

  # Ensure measures is a vector
  if (is.character(measures) && length(measures) == 1) {
    measures <- c(measures)
  }

  # Calculate influence measures
  influence_list <- lapply(measures, function(m) calculate_influence_measures(data, model, measure = m))
  names(influence_list) <- measures

  # Set up the plot with the first measure
  plot(influence_list[[1]], type = "h", col = "blue", lty = 1, ylab = "Influence Measure", xlab = "Observation",
       main = "Influence Measures", ylim = range(unlist(influence_list)))

  # Overlay additional measures if any
  if (length(influence_list) > 1) {
    for (i in 2:length(influence_list)) {
      lines(influence_list[[i]], type = "h", col = i + 1, lty = i)
    }
  }

  # Add legend
  legend("topright", legend = measures, col = 1:length(measures), lty = 1:length(measures))
}
