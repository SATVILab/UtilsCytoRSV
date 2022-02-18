test_that("subtract_background", {
  .data_test <- data.frame(
    pid = rep(c("a", "b"), each = 3),
    stim = c("mtb", "ebv", "uns") %>%
      c("uns", "ebv", "mtb"),
    resp1 = 1:6,
    resp2 = 17:12 * 2
  )

  data_out <- subtract_background(
    .data = .data_test,
    grp = "pid",
    stim = "stim",
    uns = "uns",
    resp = c("resp1", "resp2"),
    remove_uns = FALSE
  )
  expect_identical(
    data_out$resp1,
    c(-2L, -1L, 0L, 0L, 1L, 2L)
  )
  expect_identical(
    data_out$resp2,
    c(4, 2, 0, 0, -2, -4)
  )

  data_out <- subtract_background(
    .data = .data_test,
    grp = "pid",
    stim = "stim",
    uns = "uns",
    resp = c("resp1", "resp2"),
    remove_uns = TRUE
  )
  expect_identical(
    data_out$resp1,
    c(-2L, -1L, 1L, 2L)
  )
  expect_identical(
    data_out$resp2,
    c(4, 2, -2, -4)
  )

  expect_error(
    subtract_background(
      .data = .data_test %>% dplyr::bind_rows(.data_test[1:3, ]),
      grp = "pid",
      stim = "stim",
      uns = "uns",
      resp = c("resp1", "resp2"),
      remove_uns = FALSE
    )
  )
})
