test_that("plot_cyto works", {

  # prep data
  data('GvHD', package = 'flowCore')
  fr <- GvHD[[1]]
  adf <- flowCore::parameters(fr)@data
  lab_vec <- setNames(adf$desc, adf$name)
  for(i in seq_along(lab_vec)){
    if(is.na(lab_vec[i])){
      lab_vec[i] <- names(lab_vec)[i]
    }
  }
  ex_tbl <- flowCore::exprs(GvHD[[1]]) %>%
    tibble::as_tibble()
  try(detach('package:flowCore', unload = TRUE),
      silent = TRUE)
  data <- ex_tbl
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

  # expand_limits
  # -----------------

  # one element of length 1, no name
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    expand_limits = list(-1e4)
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
    expand_limits = list(1e4, -5e2)
  ))

  # one element, no name
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    expand_limits = list(c(1e4, -5e2))
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
    expand_limits = list(x = c(1e4, -5e2))
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
    expand_limits = list(y = c(1e4, -5e2))
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
    expand_limits = list(y = c(1e4, -5e2),
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
    axis_range_equal = TRUE
  )

  expect_identical(
    p$layers[[2]]$data[,1],
    p$layers[[2]]$data[,2]
  )

  # with expand_limits
  # just axis range equal
  p <- plot_cyto(
    data = ex_tbl,
    marker = marker,
    axis_range_equal = TRUE,
    expand_limits = list(y = c(1000, 200),
                         x = c(-1e4, 500))
  )

  expect_identical(
    p$layers[[2]]$data[1,] %>%
      as.numeric(),
    c(1, -1e4)
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
    axis_range_equal = TRUE,
    expand_limits = list(y = c(1e4, 200))
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
    axis_range_equal = TRUE,
    expand_limits = list(x = c(1e4, 200))
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

