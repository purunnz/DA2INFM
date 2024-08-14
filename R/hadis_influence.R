# R/hadis_influence.R
#' Custom Hadi's Influence Measure
#'
#' This function calculates Hadi's Influence Measure for each observation in a linear model.
#'
#' @param model A linear model object of class 'lm'.
#' @return A numeric vector of Hadi's Influence Measure values for each observation.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' hadi_influence_values <- hadis_influence_measure(model)
#' print(hadi_influence_values)
#' @export
hadis_influence_measure <- function(model) {
  # Check if the input is a linear model
  if (!inherits(model, "lm")) {
    stop("The input model must be of class 'lm'.")
  }

  # Extract model components
  X <- model.matrix(model)
  y <- model.response(model.frame(model))
  n <- nrow(X)
  p <- length(coef(model))  # Number of predictors

  # Calculate the hat matrix H = X(X'X)^(-1)X'
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  h <- diag(H)  # Leverage values

  # Calculate residuals
  e <- residuals(model)

  # Mean Squared Error (MSE) of the residuals
  mse <- sum(e^2) / df.residual(model)

  # Calculate the first term of Hadi's Influence Measure
  first_term <- ((p + 1) * e^2) / ((1 - h) * ((mse * (n - p - 1)) - e^2))

  # Calculate the second term of Hadi's Influence Measure
  second_term <- h / (1 - h)

  # Calculate Hadi's Influence Measure
  hadi_influence <- first_term + second_term

  return(hadi_influence)
}
#
# # Example usage:
# # Load the mtcars dataset
# data(mtcars)
#
# # Fit a linear model
# model <- lm(mpg ~ wt + hp, data = mtcars)
#
# # Calculate Hadi's Influence Measure using the custom function
# hadi_influence_values <- custom_hadis_influence_measure(model)
#
# # Print the Hadi's Influence Measure values
# print(hadi_influence_values)
#
# # Calculate Hadi's Influence Measure using the custom function
# builtin_hadi_influence_values <- ols_hadi(model)
#
# # Print the Hadi's Influence Measure values
# print(builtin_hadi_influence_values)
#
#
# # Custom Hadi's Influence Measure function (from previous example)
# custom_hadis_influence_measure <- function(model) {
#   if (!inherits(model, "lm")) stop("Model must be an object of class 'lm'.")
#
#   # Extract model components
#   X <- model.matrix(model)
#   y <- model.response(model.frame(model))
#   n <- nrow(X)
#   p <- length(coef(model))
#
#   # Calculate the hat matrix H
#   H <- X %*% solve(t(X) %*% X) %*% t(X)
#   h <- diag(H)  # Leverage values (diagonal elements of H)
#
#   # Calculate residuals
#   e <- residuals(model)
#
#   # Calculate the Mean Squared Error (MSE)
#   mse <- sum(e^2) / df.residual(model)
#
#   # Calculate standardized residuals
#   standardized_residuals <- e / sqrt(mse)
#
#   # Calculate Hadi's Influence Measure
#   hadi_influence <- numeric(n)
#   for (i in 1:n) {
#     # Leverage component
#     leverage_component <- h[i] / (1 - h[i])
#
#     # Residual component
#     residual_component <- (standardized_residuals[i]^2 * h[i]) / (1 - h[i])
#
#     # Combine both components to get Hadi's Influence Measure
#     hadi_influence[i] <- leverage_component + residual_component
#   }
#
#   return(hadi_influence)
# }
#
# # Example usage:
# data(mtcars)
#
# # Fit a linear model
# model <- lm(mpg ~ wt + hp, data = mtcars)
#
# # Calculate Hadi's Influence Measure using the custom function
# custom_hadi_influence_values <- custom_hadis_influence_measure(model)
#
# # Assign names to the custom Hadi's Influence Measure values
# names(custom_hadi_influence_values) <- rownames(mtcars)
#
# # Print the custom Hadi's Influence Measure values in the desired format
# print(custom_hadi_influence_values)


# #' Custom Hadi's Influence Measure
# #'
# #' This function calculates Hadi's Influence Measure for a given linear model.
# #'
# #' @param model A linear model object of class 'lm'.
# #'
# #' @return A numeric vector of Hadi's Influence Measure values for each observation in the model.
# #' @export
# custom_hadis_influence_measure <- function(model) {
#   # Extract model components
#   r <- rstudent(model)  # Studentized residuals
#   h <- hatvalues(model)  # Leverage values (diagonal of the hat matrix)
#   p <- length(coef(model))  # Number of parameters
#   n <- nrow(model.matrix(model))  # Number of observations
#
#   # Calculate Hadi's Influence Measure
#   Hadi <- h / (1 - h) + r^2 / (n - p)
#
#   return(Hadi)
# }


