test_that("slice_diffs_both works", {
  comp <- compare(test_df_a, test_df_b, by = car)
  out <- slice_diffs_both(test_df_a, test_df_b, comp, mpg)
  expect_snapshot(as_tibble(out))
})
