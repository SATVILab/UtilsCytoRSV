test_that("sum_over_markers works", {
  data("data_count")
  data_test <- data_count[c(1:5, 60:64), ] |>
    calc_prop(
      den = "count_pop_den",
      num = "count_pop_num"
    ) |>
    dplyr::select(-c(count_pop_den, count_pop_num)) |>
    dplyr::arrange(SubjectID, VisitType, stim, cyt_combn)

  data_out <- sum_over_markers(
    .data = data_test,
    grp = c("SubjectID", "VisitType", "stim"),
    cmbn = "cyt_combn",
    markers_to_sum = c("IFNg", "IL2", "IL17"),
    levels = c("-", "+"),
    resp = "prop"
  )

  data_out <- sum_over_markers(
    .data = data_test,
    grp = c("SubjectID", "VisitType", "stim"),
    cmbn = "cyt_combn",
    markers_to_sum = NULL,
    levels = c("-", "+"),
    resp = "prop"
  )

  expect_identical(
    setNames(data_out$cyt_combn, NULL),
    rep("", 4)
  )
  expect_equal(
    signif(setNames(data_out$prop, NULL), 6),
    c(0.999889, 0.998894, 0.999691, 0.999273)
  )

  data_out <- sum_over_markers(
    .data = data_test |> dplyr::mutate(prop2 = 1:10 / 1e3),
    grp = c("SubjectID", "VisitType", "stim"),
    cmbn = "cyt_combn",
    markers_to_sum = NULL,
    levels = c("-", "+"),
    resp = c("prop", "prop2")
  )

  expect_identical(
    setNames(data_out$cyt_combn, NULL),
    rep("", 4)
  )
  expect_equal(
    signif(setNames(data_out$prop, NULL), 6),
    c(0.999889, 0.998894, 0.999691, 0.999273)
  )
  expect_equal(
    signif(setNames(data_out$prop2, NULL), 6),
    c(0.021, 0.007, 0.008, 0.019)
  )
})
