# R/dffits.R
#' Custom DFFITS Measure
#'
#' This function calculates the DFFITS values for each observation in a linear model without using the built-in dffits function.
#'
#' @param model A linear model object of class 'lm'.
#' @return A numeric vector of DFFITS values for each observation in the model.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' dffits_values <- dffits_measure(model)
#' print(dffits_values)
#' @export
dffits_measure <- function(model) {
  if (!inherits(model, "lm")) stop("Model must be an object of class 'lm'.")

  # Extract model components
  X <- model.matrix(model)
  y <- model.response(model.frame(model))
  n <- nrow(X)
  p <- length(coef(model))  # Number of predictors

  # Calculate the hat matrix H = X(X'X)^(-1)X'
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  h <- diag(H)  # Leverage values

  # Calculate the residuals
  e <- residuals(model)

  # Calculate the Mean Squared Error (MSE)
  mse <- sum(e^2) / df.residual(model)

  # Calculate studentized residuals using custom function
  rstudent_values <- rstudent(model)

  # Calculate DFFITS
  dffits_values <- rstudent_values * sqrt(h / (1 - h))

  return(dffits_values)
}
#
# # Example usage:
# # Load the mtcars dataset
# data(mtcars)
#
# # Fit a linear model
# model <- lm(mpg ~ wt + hp, data = mtcars)
#
# # Calculate DFFITS using the custom function
# custom_dffits_values <- custom_dffits_measure(model)
#
# # Print the DFFITS values
# print(custom_dffits_values)
#
# # Calculate DFFITS using the custom function
# builtin_dffits_values <- dffits_measure(model)
#
# # Print the DFFITS values
# print(builtin_dffits_values)
#
#
#
#
