#' Calculate Influence Measures
#'
#' This function calculates influence measures for a given linear model.
#' The user can select which measure to calculate: Cook's Distance, DFFITS, or Hadi's Influence Measure.
#'
#' @param data A data frame or matrix containing the data used in the model.
#' @param model A linear model object of class 'lm'.
#' @param measure A character string specifying the influence measure to calculate.
#'        Options are "cooks", "dffits", or "hadi". Default is "cooks".
#'
#' @return A numeric vector of the selected influence measure values for each observation in the model.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' influence_values <- calculate_influence_measures(mtcars, model, measure = "cooks")
#' print(influence_values)
calculate_influence_measures <- function(data, model, measure = "cooks") {

  # Input validation
  validate_inputs(data, model)

  # # Input validation
  # if (!inherits(model, "lm")) stop("Model must be an object of class 'lm'.")
  # if (!is.data.frame(data) && !is.matrix(data)) stop("Data must be a data frame or matrix.")
  # if (any(is.na(data))) stop("Data contains NA values.")
  # # if (any(is.infinite(data))) stop("Data contains infinite values.")
  #
  # Check if the measure is valid
  valid_measures <- c("cooks", "dffits", "hadi")
  if (!measure %in% valid_measures) {
    stop("Invalid measure. Choose from 'cooks', 'dffits', or 'hadi'.")
  }

  # Perform the selected influence measure calculation
  if (measure == "cooks") {
    # influence_values <- cooks.distance(model)
    influence_values <- cooks_distance(model)
  } else if (measure == "dffits") {
    # influence_values <- dffits(model)
    influence_values <- dffits_measure(model)
  } else if (measure == "hadi") {
    # influence_values <- custom_hadis_influence_measure(model)
    influence_values <- hadis_influence_measure(model)
  }

  return(influence_values)
}
