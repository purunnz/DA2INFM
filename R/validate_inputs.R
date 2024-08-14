#' Validate Inputs for Influence Measure Calculations
#' @importFrom stats coef cooks.distance df.residual dffits formula hatvalues model.frame model.matrix model.response residuals rstudent
#' @importFrom graphics legend lines
#'
#' This function validates the inputs for calculating influence measures on a linear model.
#' It checks if the provided model is of class 'lm', and ensures that the data does not contain
#' NA or infinite values.
#'
#' @param data A data frame or matrix containing the data used in the model.
#' @param model A linear model object of class 'lm'.
#'
#' @return NULL. This function is used for its side effect of stopping execution if inputs are invalid.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' validate_inputs(mtcars, model)
validate_inputs <- function(data, model) {
  # Check if the model is of class 'lm'
  if (!inherits(model, "lm")) {
    stop("Model must be an object of class 'lm'.")
  }

  # Check if the data is a data frame or matrix
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or matrix.")
  }

  # Check if the data contains NA values
  if (any(sapply(data, function(x) any(is.na(x))))) {
    stop("Data contains NA values.")
  }

  # Check if the data contains infinite values
  if (any(sapply(data, function(x) any(is.infinite(x))))) {
    stop("Data contains infinite values.")
  }

  # Check if the data contains the variables used in the model
  model_vars <- all.vars(formula(model))
  missing_vars <- setdiff(model_vars, colnames(data))

  if (length(missing_vars) > 0) {
    stop(paste("Data is missing the following variables required by the model:",
               paste(missing_vars, collapse = ", ")))
  }

  # valid_measures <- c("cooks", "dffits", "hadi")
  # if (!measure %in% valid_measures) {
  #   stop("Invalid measure. Choose from 'cooks', 'dffits', or 'hadi'.")
  # }

}
