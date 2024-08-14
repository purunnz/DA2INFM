# tests/testthat/test-measures.R

library(testthat)

test_that("Cook's Distance is calculated correctly", {
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  cooksD <- cooks_distance(model)
  expect_length(cooksD, nrow(mtcars))
  expect_true(all(cooksD >= 0))
})

test_that("DFFITS is calculated correctly", {
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  dffits_values <- dffits_measure(model)
  expect_length(dffits_values, nrow(mtcars))
})

test_that("Hadi's Influence Measure is calculated correctly", {
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  hadi_values <- hadis_influence_measure(model)
  expect_length(hadi_values, nrow(mtcars))
})
