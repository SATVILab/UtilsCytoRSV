test_that("plot_cyto works", {

  # prep data
  data('GvHD', package = 'flowCore')
  ex_tbl <- flowCore::exprs(GvHD[[1]]) %>%
    tibble::as_tibble()
  lab_vec <- chnl_lab(data = GvHD)

  marker <- c("FL2-H", "FL3-H")

  # tests
  # ----------------

  # basic run
  expect_identical(
    class(
      plot_cyto(
        data = ex_tbl,
        marker = marker
      )
    ),
    c("gg", "ggplot")
    )

  # relabelling
  lab_list <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    lab = lab_vec
  )$labels[c('x', 'y')]
  expect_identical(
    lab_list$x[[1]],
    "CD45 PE"
  )
  expect_identical(
    lab_list$y[[1]],
    "CD14 PerCP"
  )

  # font size
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    font_size = 30
  )
  expect_equal(
    round(p$theme$axis.text$size),
    26
  )

  # limits_expand
  # -----------------

  # one element of length 1, no name
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(-1e4)
  )
  expect_identical(
    length(p$layers),
    2L
  )
  expect_identical(
    p$layers[[2]]$data,
    data.frame(
      x = c(-1e4, -1e4),
      y = c(-1e4, -1e4)
    )
  )

  # two elements, no name
  expect_error(plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(1e4, -5e2)
  ))

  # one element, no name
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(c(1e4, -5e2))
  )
  expect_identical(
    p$layers[[2]]$data,
    data.frame(
      x = c(-5e2, 1e4),
      y = c(-5e2, 1e4)
    )
  )

  # one element, one name
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(x = c(1e4, -5e2))
  )
  expect_identical(
    p$layers[[2]]$data,
    data.frame(
      x = c(-5e2, 1e4)
    )
  )
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(y = c(1e4, -5e2))
  )
  expect_identical(
    p$layers[[2]]$data,
    data.frame(
      y= c(-5e2, 1e4)
    )
  )

  # two elements, both named
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_expand = list(y = c(1e4, -5e2),
                         x = c(-1e4, 2e4))
  )
  expect_identical(
    p$layers[[2]]$data,
    data.frame(
      y = c(-5e2, 1e4),
      x = c(-1e4, 2e4)
    )
  )

  # axis range equal
  # --------------------

  # just axis range equal
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_equal = TRUE
  )

  expect_identical(
    p$layers[[2]]$data[,1],
    p$layers[[2]]$data[,2]
  )

  # with limits_expand
  # just axis range equal
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_equal = TRUE,
    limits_expand = list(y = c(1000, 200),
                         x = c(-1e4, 500))
  )

  expect_identical(
    p$layers[[2]]$data[1,] %>%
      as.numeric(),
    c(-1e4, -1e4)
  )
  expect_identical(
    p$layers[[2]]$data[2,] %>%
      as.numeric() %>%
      round(),
    c(9222, 9222)
  )

  # just y-axis
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_equal = TRUE,
    limits_expand = list(y = c(1e4, 200))
  )

  expect_identical(
    p$layers[[2]]$data[1,] %>%
      as.numeric(),
    c(1, 1)
  )
  expect_identical(
    p$layers[[2]]$data[2,] %>%
      as.numeric() %>%
      round(),
    c(1e4, 9222)
  )

  # just x-axis
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    limits_equal = TRUE,
    limits_expand = list(x = c(1e4, 200))
  )

  expect_identical(
    p$layers[[2]]$data[1,] %>%
      as.numeric(),
    c(1, 1)
  )
  expect_identical(
    p$layers[[2]]$data[2,] %>%
      as.numeric() %>%
      round(),
    c(1e4, 9222)
  )
})

test_that("plot_cyto_grid works", {
  # prep data
  data('GvHD', package = 'flowCore')
  ex_tbl <- flowCore::exprs(GvHD[[1]]) %>%
    tibble::as_tibble()
  lab_vec <- chnl_lab(data = GvHD)

  marker <- c("FL2-H", "FL3-H")

})
