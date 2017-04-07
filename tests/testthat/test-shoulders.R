context("shoulders")

test_that("identifies namespace dependencies", {
  result <- shoulders("namespace")
  expect_true(any(grepl("base", result$packages)))
})

test_that("identifies library dependencies", {
  result <- shoulders("library")
  expect_true(any(grepl("base", result$packages)))
})

test_that("identifies package dependencies", {
  skip("skip as no default cran mirror is set")
  result <- shoulders("package", "testthat", include_dependencies = TRUE)
  expect_true(any(grepl("methods", result$packages)))
})
