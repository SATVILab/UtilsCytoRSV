get_axis_equalise_limits <- function(data, facet_level_combn_tbl, marker,
                                     limits_equal_across) {
  if (limits_equal_cross == "none") {
    # now, get
  }
  if (limits_equal_across == "all") {



  }
  if (!limits_equal || limits_equal_within == "plot") {
    limits_expand_list <- purrr::map(facet_level_combn_tbl[[1]], function(level_x) {
      purrr::map(facet_level_combn_tbl[[2]], function(level_y) {
        if (is.null(limits_expand)) {
          return(NULL)
        }
        limits_expand # limits_expand is fixed to be the same for all plots
      }) |>
        setNames(facet_level_combn_tbl[[2]])
    }) |>
      setNames(facet_level_combn_tbl[[1]])
  } else {
    # now limits must be equal and not just equal within 'plot'
    if (limits_equal_within == "all") {
      # if the marker is not a facet,
      # then we can just take range of these markers
      # and expand_limits
      # these marke
      if (!"marker" %in% facet) {
        # calculate range across marker values
        range_x <- plot_tbl[[marker[1]]] |>
          c(limits_expand[["x"]]) |>
          range()
        range_y <- plot_tbl[[marker[2]]] |>
          c(limits_expand[["y"]]) |>
          range()
      } else {
        # now we need to take the range across all marker values
        marker_vec_x <- marker |>
          purrr::map(mk, mk[1]) |>
          unique()
        range_x <- purrr::map(marker_vec_x, function(mk) {
          range(data[[mk]])
        }) |>
          unlist() |>
          c(limits_expand[["x"]]) |>
          range()
        marker_vec_y <- marker |>
          purrr::map(mk, mk[2]) |>
          unique()
        range_y <- purrr::map(marker_vec_y, function(mk) {
          range(data[[mk]])
        }) |>
          unlist() |>
          c(limits_expand[["x"]]) |>
          range()
      }
      # create list, one list(x,y) for each combination
      # of the facet levels
      limits_expand_list <- purrr::map(facet_level_combn_tbl[[1]], function(level_x) {
        purrr::map(facet_level_combn_tbl[[2]], function(level_y) {
          list(x = range_x, y = range_y)
        }) |>
          setNames(facet_level_combn_tbl[[2]])
      }) |>
        setNames(facet_level_combn_tbl[[1]])
    } else if (limits_equal_within %in% c("row", "col")) {
      # so now, both plot dims equal within row or col
      if (!"marker" %in% facet) {
        limits_expand_list <- purrr::map(facet_level_combn_tbl[[1]], function(level_x) {
          purrr::map(facet_level_combn_tbl[[2]], function(level_y) {
            lim_tbl <- switch(limits_equal_within,
              "x" = data[data[[facet["x"]]] == level_x, ],
              "y" = data[data[[facet["y"]]] == level_y, ]
            )
          }) |>
            setNames(facet_level_combn_tbl[[2]])
        }) |>
          setNames(facet_level_combn_tbl[[1]])
      }
    } else if (limits_equal_within == "col") {
      x
    }
  }
}
