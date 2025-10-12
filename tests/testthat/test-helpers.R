test_that("shorten_works", {
  # length 1 input that is not too long
  expect_identical(
    shorten(c(a = "12345"), 5),
    c(a = "12345")
  )
  # length 1 input that is too long
  expect_identical(
    shorten(c(a = "123456"), 5),
    c(a = "12...")
  )
  # length > 1 input with some too-long elements
  expect_identical(
    shorten(c(a = "a", b = glue_collapse(1:10, ""), c = "c")),
    c(a = "a", b = "1234567...", c = "c")
  )
  # length > 1 input with no too-long elements
  expect_identical(
    shorten(setNames(letters, letters)),
    setNames(letters, letters)
  )
  # errors when not character
  expect_error(shorten(1, 10), regexp = "is.character\\(x\\) is not TRUE")
})

test_that("dottize works", {
  expect_identical(
    dottize(character(0), 8),
    character(0)
  )
  x <- "123456789"
  expect_identical(
    dottize(c(x, letters), 8),
    glue("12345...")
  )
  expect_identical(
    dottize(x, 100),
    glue(x)
  )
  expect_identical(
    dottize(c(x, letters), 10),
    glue("123456789, ...")
  )
  expect_identical(
    dottize(names(mtcars), 30),
    glue("mpg, cyl, disp, hp, drat, ...")
  )
})

test_that("ensure_ptype_compatible() works", {
  needing_coerce <- list(a = test_df_a, b = test_df_b) %>%
    map(fsubset, j = setdiff(names(test_df_a), "extracol_a"))
  post_coerce <- needing_coerce %>%
    map(\(x) mutate(x, wt = as.character(wt)))

  # when coercion is needed
  expect_identical(
    ensure_ptype_compatible(needing_coerce),
    post_coerce
  )

  # when coercion is not needed
  expect_identical(
    ensure_ptype_compatible(post_coerce),
    post_coerce
  )
})

test_that("check_table_arg respects custom table_id", {
  # Create comparisons with different table_id values
  comp_default <- compare(test_df_a, test_df_b, by = car)
  comp_custom <- compare(test_df_a, test_df_b, by = car, table_id = c("original", "updated"))

  # Default table_id - accepts "a" and "b"
  expect_silent(check_table_arg(quo("a"), comp_default))
  expect_silent(check_table_arg(quo("b"), comp_default))
  expect_snapshot(check_table_arg(quo("original"), comp_default), error = TRUE)

  # Custom table_id - accepts "original" and "updated"
  expect_silent(check_table_arg(quo("original"), comp_custom))
  expect_silent(check_table_arg(quo("updated"), comp_custom))
  expect_snapshot(check_table_arg(quo("a"), comp_custom), error = TRUE)
  expect_snapshot(check_table_arg(quo("b"), comp_custom), error = TRUE)

  # Invalid inputs work the same regardless of table_id
  expect_snapshot(check_table_arg(quo(c("a", "b")), comp_default), error = TRUE)
  expect_snapshot(check_table_arg(quo(), comp_default), error = TRUE)
})

test_that("clean_table_id strips attributes from named vectors", {
  # Named vector - attributes should be stripped
  named_vec <- c(first = "table1", second = "table2")
  expect_identical(
    clean_table_id(named_vec),
    c("table1", "table2")
  )

  # Vector with custom attributes
  vec_with_attrs <- c("x", "y")
  attr(vec_with_attrs, "custom") <- "attribute"
  attr(vec_with_attrs, "another") <- 123
  expect_identical(
    clean_table_id(vec_with_attrs),
    c("x", "y")
  )

  # Plain vector without attributes should remain unchanged
  plain_vec <- c("alpha", "beta")
  expect_identical(
    clean_table_id(plain_vec),
    plain_vec
  )
})
