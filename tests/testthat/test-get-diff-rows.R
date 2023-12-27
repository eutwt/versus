test_that("get_diff_rows_dbl()", {
  empty <- tibble(row_a = integer(0), row_b = integer(0))
  one_one <- tibble(row_a = 1L, row_b = 1L)

  expect_identical(cpp_get_diff_rows(1.1, 1.1, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(NA_real_, NA_real_, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(NA_real_, 1.1, 1L, 1L), one_one)
  expect_identical(cpp_get_diff_rows(2.1, 1.1, 1L, 1L), one_one)
})

test_that("get_diff_rows_dbl() works for dates", {
  empty <- tibble(row_a = integer(0), row_b = integer(0))
  one_one <- tibble(row_a = 1L, row_b = 1L)
  date_a <- as.Date("2023-01-01")
  date_b <- as.Date("2023-01-02")
  date_na <- as.Date(NA)

  expect_identical(cpp_get_diff_rows(date_a, date_a, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(date_na, date_na, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(date_na, date_a, 1L, 1L), one_one)
  expect_identical(cpp_get_diff_rows(date_a, date_b, 1L, 1L), one_one)
})

test_that("get_diff_rows_int()", {
  empty <- tibble(row_a = integer(0), row_b = integer(0))
  one_one <- tibble(row_a = 1L, row_b = 1L)

  expect_identical(cpp_get_diff_rows(1L, 1L, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(NA_integer_, NA_integer_, 1L, 1L), empty)
  expect_identical(cpp_get_diff_rows(NA_integer_, 1L, 1L, 1L), one_one)
  expect_identical(cpp_get_diff_rows(2L, 1L, 1L, 1L), one_one)
})
