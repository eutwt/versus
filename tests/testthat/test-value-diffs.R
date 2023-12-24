test_that("value_diffs with a single column works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, mpg))
})

test_that("value_diffs works when the supplied columns have no diffs ", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, hp))
  expect_snapshot(value_diffs_stacked(comp, c(hp, drat)))
})

test_that("Error when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(
    value_diffs(mtcars, mpg),
    error = TRUE
  )
  expect_snapshot(
    value_diffs_stacked(mtcars, c(mpg, disp)),
    error = TRUE
  )
})

test_that("Error on value_diffs with empty selection", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, where(is.factor)), error = TRUE)
  expect_snapshot(value_diffs_stacked(comp, where(is.factor)), error = TRUE)
})

test_that("Error on value_diffs when column doesn't exist", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, bear), error = TRUE)
  expect_snapshot(value_diffs_stacked(comp, c(bear, mpg)), error = TRUE)
})

test_that("Error on value_diffs() with multiple columns", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, c(mpg, disp)), error = TRUE)
})

test_that("value_diffs_stacked() works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  # using c()
  expect_snapshot(value_diffs_stacked(comp, c(mpg, disp)))
  # using where()
  expect_snapshot(value_diffs_stacked(comp, where(is.numeric)))
})

test_that("value_diffs_stacked() works without column", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs_stacked(comp))
})

test_that("value_diffs_stacked() coerces to char on incompatible ptypes", {
  test_df_a_char_mpg <- test_df_a %>%
    mutate(mpg = as.character(mpg))
  comp <- compare(test_df_a_char_mpg, test_df_b, by = car)
  expect_snapshot(value_diffs_stacked(comp))
})
