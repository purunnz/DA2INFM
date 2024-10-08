% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_infm.R
\name{calculate_influence_measures}
\alias{calculate_influence_measures}
\title{Calculate Influence Measures}
\usage{
calculate_influence_measures(data, model, measure = "cooks")
}
\arguments{
\item{data}{A data frame or matrix containing the data used in the model.}

\item{model}{A linear model object of class 'lm'.}

\item{measure}{A character string specifying the influence measure to calculate.
Options are "cooks", "dffits", or "hadi". Default is "cooks".}
}
\value{
A numeric vector of the selected influence measure values for each observation in the model.
}
\description{
This function calculates influence measures for a given linear model.
The user can select which measure to calculate: Cook's Distance, DFFITS, or Hadi's Influence Measure.
}
\examples{
model <- lm(mpg ~ wt + hp, data = mtcars)
influence_values <- calculate_influence_measures(mtcars, model, measure = "cooks")
print(influence_values)
}
