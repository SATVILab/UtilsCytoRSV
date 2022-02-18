test_that("chnl_lab works", {
  data("GvHD", package = "flowCore")
  lab_vec <- chnl_lab(GvHD)
  expect_identical(
    lab_vec[[1]],
    c("FSC-Height")
  )
  expect_identical(
    names(lab_vec)[3],
    c("FL1-H")
  )
  lab_vec <- chnl_lab(GvHD[[1]])
  expect_identical(
    lab_vec[[1]],
    c("FSC-Height")
  )
  expect_identical(
    names(lab_vec)[3],
    c("FL1-H")
  )
})


test_that("marker_lab works", {
  data("GvHD", package = "flowCore")
  lab_vec <- marker_lab(GvHD)
  expect_identical(
    lab_vec[[1]],
    "FSC-H"
  )
  expect_identical(
    names(lab_vec)[3],
    "CD15 FITC"
  )
})
