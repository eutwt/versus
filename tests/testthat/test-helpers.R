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
