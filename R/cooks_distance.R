# R/cooks_distance.R
#' Custom Cook's Distance Measure
#'
#' This function calculates Cook's Distance for a linear model object without using the built-in cooks.distance function.
#'
#' @param model A linear model object of class 'lm'.
#' @return A numeric vector of Cook's Distance values for each observation in the model.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' cooksD <- cooks_distance(model)
#' print(cooksD)
#' @export
#--------------------------
cooks_distance <- function(model) {
  if (!inherits(model, "lm")) {
    stop("The input model must be of class 'lm'.")
  }

  # Extract model components
  X <- model.matrix(model)
  y <- model.response(model.frame(model))
  n <- nrow(X)
  p <- length(coef(model))

  # Calculate the hat matrix H
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  h <- diag(H)  # Leverage values

  # Calculate residuals
  e <- residuals(model)

  # Calculate the mean squared error (MSE)
  mse <- sum(e^2) / df.residual(model)

  # Calculate Cook's Distance
  cooksD <- (e^2 / (p * mse)) * (h / (1 - h)^2)

  return(cooksD)
}



#' Custom Leverage Values (Hat Matrix Diagonal)
#'
#' This function calculates the leverage values for each observation in a linear model.
#'
#' @param model A linear model object of class 'lm'.
#' @return A numeric vector of leverage values for each observation.
#' @export
custom_hatvalues <- function(model) {
  # Extract the design matrix (X matrix) from the model
  X <- model.matrix(model)

  # Calculate the hat matrix H = X(X'X)^(-1)X'
  H <- X %*% solve(t(X) %*% X) %*% t(X)

  # Leverage values are the diagonal elements of the hat matrix
  h <- diag(H)

  return(h)
}

#' Custom Studentized Residuals
#'
#' This function calculates studentized residuals for a linear model object.
#'
#' @param model A linear model object of class 'lm'.
#' @return A numeric vector of studentized residuals for each observation.
#' @export
custom_rstudent <- function(model) {
  # Ordinary residuals
  e <- residuals(model)

  # Mean Squared Error (MSE) of the residuals
  mse <- sum(e^2) / df.residual(model)

  # Calculate leverage values using the custom function
  h <- custom_hatvalues(model)

  # Calculate studentized residuals
  r <- e / sqrt(mse * (1 - h))

  return(r)
}
#
#
# # Example usage:
# # Load the mtcars dataset
# data(mtcars)
#
# # Fit a linear model
# model <- lm(mpg ~ wt + hp, data = mtcars)
#
# # Calculate Cook's Distance using the custom function
# custom_cooksD <- custom_cooks_distance(model)
#
# # Print the Cook's Distance values
# print(custom_cooksD)
#
# butilin_cooksD <- cooks_distance(model)
# print(butilin_cooksD)
#
# # Plot Cook's Distance
# plot(custom_cooksD, type = "h", main = "Custom Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
# abline(h = 4/(nrow(mtcars)-length(coef(model))), col = "red", lty = 2)
#
# # Plot Cook's Distance
# plot(butilin_cooksD, type = "h", main = "Builtin Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
# abline(h = 4/(nrow(mtcars)-length(coef(model))), col = "red", lty = 2)
#
# # Calculate the difference between the two Cook's Distance vectors
# difference <- custom_cooksD / butilin_cooksD
#
# # Print the difference
# print("Difference between Custom and Built-in Cook's Distance:")
# print(difference)


