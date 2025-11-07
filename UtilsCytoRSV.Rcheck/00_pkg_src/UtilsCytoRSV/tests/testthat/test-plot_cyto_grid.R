test_that("plot_cyto_grid works", {
  testthat::skip("plot_cyto_grid not yet working")
  .install_pkg_bioc("flowCore") # nolint
  # prep data
  data("GvHD", package = "flowCore")
  ex_tbl <- flowCore::exprs(GvHD[[1]]) |>
    tibble::as_tibble()
  lab_vec <- chnl_lab(data = GvHD)
  colnames(ex_tbl) <- lab_vec[colnames(ex_tbl)]

  marker <- list(
    c("FSC-Height", "SSC-Height"),
    c("CD15 FITC", "CD45 PE"),
    c("CD14 PerCP", "FL2-A")
  )

  # tests - basic
  # ----------------

  # basic output
  p_list_out <- plot_cyto_grid(
    data = ex_tbl,
    marker = marker,
    return_plots = TRUE,
    return_grid = TRUE
  )
  expect_identical(
    names(p_list_out),
    c("p_list", "p_grid")
  )
  expect_identical(
    length(p_list_out$p_list),
    3L
  )
  expect_identical(
    purrr::map(p_list_out$p_list, class) |>
      unlist() |>
      unique(),
    c("gg", "ggplot")
  )
  expect_identical(
    class(p_list_out$p_grid),
    c("gg", "ggplot")
  )

  expect_identical(
    length(p_list_out$p_grid),
    9L
  )

  # tests - with faceting
  # -----------------

  # prep data
  data("GvHD", package = "flowCore")
  ex_tbl <- purrr::map_df(1:3, function(i) {
    fr <- GvHD[[i]]

    flowCore::exprs(fr) |>
      tibble::as_tibble() |>
      dplyr::mutate(sampleid = fr@description$FILENAME) |>
      dplyr::select(sampleid, everything())
  })
  lab_vec <- c(lab_vec, c("sampleid" = "sampleid"))
  colnames(ex_tbl) <- lab_vec[colnames(ex_tbl)]

  marker <- list(
    c("FSC-Height", "SSC-Height"),
    c("CD15 FITC", "CD45 PE"),
    c("CD14 PerCP", "FL2-A")
  )

  plot_cyt_grid(
    data = ex_tbl,
    marker = marker,
    facet
  )
})
