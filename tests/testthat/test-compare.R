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

test_that("compare() works when table arguemnts aren't symbols", {
  comp <- compare(test_df_a %>% mutate(x = 1), test_df_b, by = car, allow_both_NA = FALSE)
  expect_equal(comp$tables$expr[1], "test_df_a %>% mutate(x = 1)")
})
