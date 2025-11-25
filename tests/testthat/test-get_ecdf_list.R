test_that("get_ecdf_expr works", {

  # set up data for testing
  expr_tbl <- tibble::tibble(
    v1 = rnorm(20),
    v2 = rnorm(20, mean = 1)
  )

  # error checks
  expect_error(
    get_ecdf_list(expr_tbl = "a")
  )
  expect_error(
    get_ecdf_list(expr_tbl = data.frame(x = rnorm(1), y = "a"))
  )

  # test for dataframe without column names
  df_no_names <- data.frame(1:5, 6:10)
  colnames(df_no_names) <- NULL
  expect_error(
    get_ecdf_list(expr_tbl = df_no_names),
    "expr_tbl must have names"
  )

  # good input output checks
  expect_identical(
    class(get_ecdf_list(expr_tbl)),
    "list"
  )
  expect_identical(
    names(get_ecdf_list(expr_tbl)),
    c("v1", "v2")
  )
})
