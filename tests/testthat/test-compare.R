test_that("Error on input with duplicates", {
  expect_snapshot(
    compare(mtcars, mtcars, by = c(disp, cyl)),
    error = TRUE
  )
})

test_that("Error on empty `by`", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = where(is.character)), error = TRUE)
})

test_that("Error when `by` columns don't match", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = where(is.numeric)), error = TRUE)
})

test_that("Error when columns in `by` aren't present", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = x), error = TRUE)
})

test_that("Error on named `by`", {
  a <- data.frame(x = 1)
  b <- data.frame(g = 2)
  expect_snapshot(compare(a, b, by = c(y = x)), error = TRUE)
})

test_that("Error when `by` uses `join_by`", {
  a <- data.frame(x = 1)
  b <- data.frame(x = 1)
  expect_snapshot(compare(a, b, by = join_by(x)), error = TRUE)
})

test_that("Error on different classes with coerce = FALSE", {
  expect_snapshot(compare(test_df_a, test_df_b, by = car, coerce = FALSE),
    error = TRUE
  )
})

test_that("example comparison", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(comp)
  expect_snapshot(value_diffs_all(comp))
})

test_that("example comparison with allow_bothNA = FALSE", {
  comp <- compare(test_df_a, test_df_b, by = car, allow_both_NA = FALSE)
  expect_snapshot(comp)
  expect_snapshot(value_diffs_all(comp))
})

test_that("value_diffs with a single column works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, mpg))
})

test_that("value_diffs with multiple columns errors", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, c(mpg, disp)), error = TRUE)
})

test_that("value_diffs_stacked works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  # using c()
  expect_snapshot(value_diffs_stacked(comp, c(mpg, disp)))
  # using where()
  expect_snapshot(value_diffs_stacked(comp, where(is.numeric)))
})

test_that("value_diffs_all coerces to char on incompatible ptypes", {
  test_df_a_char_mpg <- test_df_a %>%
    mutate(mpg = as.character(mpg))
  comp <- compare(test_df_a_char_mpg, test_df_b, by = car)
  expect_snapshot(as_tibble(value_diffs_all(comp)))
})
