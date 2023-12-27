test_that("weave_diffs_long works", {
  comp <- compare(arrange(example_df_a, desc(car)), test_df_b, by = car)
  expect_snapshot(weave_diffs_long(comp, mpg))
  expect_snapshot(weave_diffs_long(comp, c(mpg, wt)))
  expect_snapshot(weave_diffs_long(comp, c(mpg, disp)))
})

test_that("weave_diffs_wide works", {
  comp <- compare(example_df_a, test_df_b, by = car)
  expect_snapshot(weave_diffs_wide(comp, mpg))
  expect_snapshot(weave_diffs_wide(comp, c(mpg, wt)))
  expect_snapshot(weave_diffs_wide(comp, c(mpg, disp)))
})

test_that("Error when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(weave_diffs_long(example_df_a, disp), error = TRUE)
  expect_snapshot(weave_diffs_wide(example_df_a, disp), error = TRUE)
})

test_that("Error when `column` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(weave_diffs_long(example_df_a, disp), error = TRUE)
  expect_snapshot(weave_diffs_wide(example_df_a, disp), error = TRUE)
})

test_that("Error on weave_diffs() with empty selection", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(weave_diffs_long(comp, where(is.factor)), error = TRUE)
  expect_snapshot(weave_diffs_wide(comp, where(is.factor)), error = TRUE)
})

test_that("Error on value_diffs when column doesn't exist", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(weave_diffs_long(comp, bear), error = TRUE)
  expect_snapshot(weave_diffs_wide(comp, bear), error = TRUE)
})
