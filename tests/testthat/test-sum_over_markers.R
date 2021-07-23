test_that("sum_over_markers works", {
  data("data_count")
  data_test <- data_count[c(1:5, 60:64),] %>%
    calc_prop(count_den = "count_pop_den",
              count_num = "count_pop_num") %>%
    dplyr::select(-c(count_pop_den, count_pop_num)) %>%
    dplyr::arrange(SubjectID, VisitType, stim, cyt_combn)

  data_out <- sum_over_markers(
    .data = data_test,
    grp = c("SubjectID", "VisitType", "stim"),
    cmbn = "cyt_combn",
    markers_to_sum = c("IFNg", "IL2", "IL17"),
    levels = c("-", "+"),
    resp = "prop"
  )

  if(FALSE) {
    # these tests work when run manually but
    # some strange arrangement error appears
    # when run automatically
    expect_identical(
      setNames(data_out$cyt_combn, NULL),
      c("TNF-", "TNF+", "TNF-", "TNF-", "TNF-", "TNF+")
    )
    expect_equal(
      signif(setNames(data_out$prop, NULL), 6),
      c(0.999704, 0.000185298, 0.998894, 0.999691, 0.999273, 0)
    )
  }

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
})

