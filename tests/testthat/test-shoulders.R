context("shoulders")

test_that("identifies namespace dependencies", {
  result <- shoulders("session")
  expect_true(any(grepl("base", result$packages)))
})

test_that("shoulders defaults to session", {
  expect_equal(shoulders("session"), shoulders())
})

test_that("thankr is not part of the session pkg list", {
  expect_true(all(shoulders("session")$packages != "thankr"))
})

test_that("identifies library dependencies", {
  result <- shoulders("library")
  expect_true(any(grepl("base", result$packages)))
})

test_that("identifies package dependencies", {
  skip_on_cran()
  options(repos = c("CRAN" = "https://cloud.r-project.org"))
  result <- shoulders("package", "testthat", include_dependencies = TRUE)
  expect_true(any(grepl("methods", result$packages)))
})

test_that("identifies package dependencies without dependencies", {
  result <- shoulders("package", "testthat", include_dependencies = FALSE)
  expect_equal("testthat", result$packages)
})

test_that("shoulders fails if where parameter is not correct", {
  expect_error(shoulders("wat"))
})

test_that("shoulders fails if package lookup done without a package", {
  expect_error(shoulders("package"), "specify")
})

test_that("shoulders fails if all packages do not exist", {
  expect_error(shoulders("package", "watwatwatwatwat"), "exist")
})

test_that("shoulders ignores packages that do not exists", {
  expect_warning(result <- shoulders("package", c("base", "watwatwatwatwat")),
                           "watwatwatwatwat")
  expect_equal(1, nrow(result))
})
