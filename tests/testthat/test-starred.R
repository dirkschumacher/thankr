context("starred")
test_that("can extract repository and username from urls", {
  urls <- c(
    "https://github.com/xyz-3/abc#readme",
    "http://github.com/xy-z/abc2/",
    "https://github.com/xyz2/abc"
  )
  result <- extract_user_repo(urls)
  expect_equal(result$username, c("xyz-3", "xy-z", "xyz2"))
  expect_equal(result$repository, c("abc", "abc2", "abc"))
})

test_that("we can correctly identify a github url", {
  expect_true(url_is_github("http://github.com"))
  expect_true(url_is_github("https://github.com"))
  expect_false(url_is_github("http://not-github.com/github.com"))
})

test_that("we can extract a package url", {
  expect_equal(NA_character_, get_package_url(pkg_name = "base"))
  expect_true(!is.na(get_package_url(pkg_name = "testthat")))
})

