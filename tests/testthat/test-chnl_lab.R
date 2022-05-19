test_that("chnl_lab works", {
  suppressWarnings(data("GvHD", package = "flowCore"))
  
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
test_that("get_chnl works", {
  data("GvHD", package = "flowCore")
  chnl_vec <- get_chnl(GvHD)
  expect_identical(
    chnl_vec[[1]],
    "FSC-H"
  )
  testthat::expect_null(names(chnl_vec))
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

test_that("get_marker works", {
  data("GvHD", package = "flowCore")
  marker_vec <- get_marker(GvHD)
  expect_identical(
    marker_vec[[1]],
    "FSC-Height"
  )
  testthat::expect_null(names(marker_vec))
})
