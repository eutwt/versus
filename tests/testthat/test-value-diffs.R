test_that("value_diffs with a single column works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, mpg))
})

test_that("value_diffs works when the supplied columns have no diffs ", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, hp))
  expect_snapshot(value_diffs_stacked(comp, c(hp, drat)))
})

test_that("Error on value_diffs with empty selection", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, where(is.Date)), error = TRUE)
  expect_snapshot(value_diffs_stacked(comp, where(is.Date)), error = TRUE)
})

test_that("Error on value_diffs when column doesn't exist", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs(comp, bear), error = TRUE)
  expect_snapshot(value_diffs_stacked(comp, c(bear, mpg)), error = TRUE)
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

test_that("value_diffs_all() works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(value_diffs_all(comp))
})

test_that("value_diffs_all coerces to char on incompatible ptypes", {
  test_df_a_char_mpg <- test_df_a %>%
    mutate(mpg = as.character(mpg))
  comp <- compare(test_df_a_char_mpg, test_df_b, by = car)
  expect_snapshot(as_tibble(value_diffs_all(comp)))
})
