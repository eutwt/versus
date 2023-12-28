
test_that("missing values are handled correctly", {
  x <- c(NA_real_, 1, 1, NA_real_, 1)
  y <- c(NA_real_, 1, 2, 1, NA_real_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_real_, 1, 1, NA_real_, 1) %>% as.Date(origin = '1900-01-01')
  y <- c(NA_real_, 1, 2, 1, NA_real_)  %>% as.Date(origin = '1900-01-01')
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_integer_, 1L, 1L, NA_integer_, 1L)
  y <- c(NA_integer_, 1L, 2L, 1L, NA_integer_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  x <- c(NA_character_, 'a', 'a', NA_character_, 'a')
  y <- c(NA_character_, 'a', 'b', 'a', NA_character_)
  expect_identical(not_equal(x, y, TRUE), 3:5)

  a <- as.POSIXct('1900-01-01')
  b <- as.POSIXct('1900-01-01', tz = "UTC")
  na <- as.POSIXct(NA)
  x <- c(na, a, a, na, a)
  y <- c(na, a, b, a, na)
  expect_identical(not_equal(x, y, TRUE), 3:5)
})
