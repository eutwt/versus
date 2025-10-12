test_that("missing values are handled correctly", {
  x <- c(NA_real_, 1, 1, NA_real_, 1)
  y <- c(NA_real_, 1, 2, 1, NA_real_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_real_, 1, 1, NA_real_, 1) %>% as.Date(origin = "1900-01-01")
  y <- c(NA_real_, 1, 2, 1, NA_real_) %>% as.Date(origin = "1900-01-01")
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_integer_, 1L, 1L, NA_integer_, 1L)
  y <- c(NA_integer_, 1L, 2L, 1L, NA_integer_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA, TRUE, TRUE, NA, TRUE)
  y <- c(NA, TRUE, FALSE, TRUE, NA)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_character_, "a", "a", NA_character_, "a")
  y <- c(NA_character_, "a", "b", "a", NA_character_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  a <- as.POSIXct("1900-01-01", tz = "America/New_York")
  b <- as.POSIXct("1900-01-01", tz = "UTC")
  na <- as.POSIXct(NA)
  x <- c(na, a, a, na, a)
  y <- c(na, a, b, a, na)
  expect_identical(not_equal(x, y, TRUE), 3:5)
})

test_that("is_simple_class() works", {
  ex <- as.POSIXct("1900-01-01", tz = "UTC")
  expect_identical(is_simple_class(1L, 1.1), FALSE)
  expect_identical(is_simple_class(1.1, 1.1), TRUE)
  expect_identical(is_simple_class(1L, 1L), TRUE)
  expect_identical(is_simple_class("a", "a"), TRUE)
  expect_identical(is_simple_class(ex, ex), TRUE)
  expect_identical(is_simple_class(as.Date(ex), ex), FALSE)
  expect_identical(is_simple_class(as.Date(ex), as.Date(ex)), TRUE)
  expect_identical(is_simple_class(FALSE, FALSE), TRUE)
  expect_identical(is_simple_class(list(1), list(1)), FALSE)
})

test_that("get_diff_rows respects custom table_id", {
  table_a <- data.frame(x = 1:5, y = c(1, 2, 3, 4, 5))
  table_b <- data.frame(x = 1:5, y = c(1, 2, 9, 4, 5))
  matches <- list(common = data.frame(a = 1:5, b = 1:5))
  
  # Default table_id
  result_default <- get_diff_rows("y", table_a, table_b, matches, TRUE, c("a", "b"))
  expect_named(result_default, c("row_a", "row_b"))
  expect_equal(result_default$row_a, 3)
  expect_equal(result_default$row_b, 3)
  
  # Custom table_id
  result_custom <- get_diff_rows("y", table_a, table_b, matches, TRUE, c("original", "updated"))
  expect_named(result_custom, c("row_original", "row_updated"))
  expect_equal(result_custom$row_original, 3)
  expect_equal(result_custom$row_updated, 3)
})
