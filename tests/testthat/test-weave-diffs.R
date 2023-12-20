test_that("weave_diffs_long works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- weave_diffs_long(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(out)
})

test_that("weave_diffs_long works with multi-variable `by`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  out <- weave_diffs_long(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(out)
})

test_that("Error when `comparison` isn't a comparison", {
  comp <- compare(example_df_a, example_df_b, by = c(car, drat))
  expect_snapshot(
    example_df_a |> weave_diffs_long(example_df_b, disp),
    error = TRUE
  )
  expect_snapshot(
    example_df_a |> weave_diffs_long(example_df_b, 1, disp),
    error = TRUE
  )
})

test_that("Error when supplied table doesn't contain cols in `comparison`", {
  comp <- compare(test_df_a, test_df_b, by = car)
  expect_snapshot(
    weave_diffs_long(test_df_a, tibble(car = 1), comp, column = drat),
    error = TRUE
  )
})
