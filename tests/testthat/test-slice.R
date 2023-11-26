test_that("slice_diffs_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(as_tibble(out))
})

test_that("slice_diffs_both works with multi-variable `by`", {
  comp <- compare(test_df_a, test_df_b, by = c(car, vs, gear))
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(as_tibble(out))
})

test_that("slice_diffs works when there are no diffs", {
  df <- rownames_to_column(mtcars, "car")
  comp <- compare(df, df, by = "car")
  expect_identical(slice_diffs(df, comp), df[0, ])
})
