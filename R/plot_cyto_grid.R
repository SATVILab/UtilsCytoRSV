#' @title Plot cytometry data ggplot2::facet_grid-style
#'
#' @inheritParams plot_cyto
#' @param facet named character vector. If not provided, then will facet column-wise
#' across each element of \code{marker}. If provided, then may have names \code{'x'} and
#' \code{'y'}, which means that the corresponding element specifies the
#' variable that is held constant along the x- and y-axes respectively.
#' For example, if \code{facet = c('x' = 'subjectid', y = 'visittype')},
#' then each row will correspond to one subject and each column will
#' correspond to one visittype.
#' A special option is to set an element of \code{facet} to \code{'marker'}. This
#' means, in a sense, the opposite to before: now all elements in \code{marker} (which
#' must now be a list) are the values that are plotted along a given axis. FOr example,
#' if \code{facet = c('x' = 'sampleid', y = 'marker')}, then each row will
#' correspond to a particular value of 'sampleid' and each column will
#' correspond to a particular element of \code{marker}. In that case, if
#' \code{marker = list(c('v1', 'v2'), c('v3', 'v4')}, then the first
#' column will be plots of the variable \code{'V2'} against the variable \code{'V1'}. The second
#' column will be plots of the variable \code{'V4'} against the variable \code{'V3'}.
#' @param facet_subset Currently unused. Reserved for future functionality.
#' @param facet_plot Currently unused. Reserved for future functionality.
#' @param n_col Currently unused. Reserved for future functionality.
#' @param limits_equal_within_plot logical. If \code{TRUE}, then each individual plot
#' will make the ranges of the x- and y-axes equal
#' @param limits_equal_across 'all','row', 'column' or 'neither'. Specifies whether
#' limits for the x- and y-axes should be made equal across rows and columns,
#' across rows only, across columns only or across neither rows nor columns. . If \code{'all'}, then
#' every x-axis will have the same range and every y-axis will have the same range.
#' If \code{'row'}, then every x- and y-axis will have the same range within a
#' given column (because it's ACROSS rows, not within). If \code{'col'},
#' then every x- and y-axis will have the same range withn a given row. Default is \code{'neither'}.
#' @param return_grid,return_plots logical. If \code{TRUE}, then
#' the grid of plots is returned or the plots themselves are returned as a list.
#' Default is \code{return_grid = TRUE} and \code{return_plots = FALSE}.
#'
#' TODO:
#' Use ... to pass on any extra arguments to a given plotting function.
#'
#' @return list with named elements 'p_list' and/or 'p_grid', depending on
#' values of \code{return_grid} and \code{return_plots}.
#'
#' @export
# data <- ex_tbl
# lab <- NULL; limits_expand <- NULL; limits_equal_within_plot <- FALSE; font_size <- 14
# limits_equal_across <- list('x' = 'all', 'y' = 'all') # could also be 'marker' (make it the same within each marker, wherever it is)
# facet <- c("x" = 'marker', "y" = 'sampleid')
plot_cyto_grid <- function(data,
                           marker,
                           lab = NULL,
                           facet = NULL,
                           facet_subset = NULL,
                           facet_plot = NULL,
                           n_col = NULL,
                           limits_expand = NULL,
                           limits_equal_within_plot = FALSE,
                           limits_equal_across = c(
                             "x" = "all",
                             "y" = "all"
                           ),
                           font_size = 14,
                           return_plots = FALSE,
                           return_grid = TRUE) {
  # checks
  # --------------------
  if (!return_grid && !return_plots) {
    stop("At least one of return_grid or return_plots must be TRUE")
  }

  if (!"marker" %in% facet) {
    plot_tbl <- data[, c(unlist(marker), facet)]
    marker_vec <- unlist(marker)
  }

  if (!is.null(limits_expand)) {
    if (is.null(names(limits_expand))) {
      limits_expand <- list(
        x = limits_expand[[1]],
        y = limits_expand[[1]]
      )
    }
  }



  purrr::map(facet_level_combn_tbl[[facet["x"]]], function(level_x) {
    if (facet["x"] == "marker") {
      marker_vec <- stringr::str_split(level_x, pattern = "~~~")[[1]]
      plot_tbl <- data[, c(marker_vec, facet[!facet == "marker"])]
    }
    purrr::map(facet_level_combn_tbl[[facet["y"]]], function(level_y) {
      if (facet["y"] == "marker") {
        marker_vec <- stringr::str_split(level_y, pattern = "~~~")[[1]]
        plot_tbl <- data[, c(marker_vec, facet[!facet == "marker"])]
      }
      facet_marker_non <- facet[!facet == "marker"]
      level_vec <- c("x" = level_x, "y" = level_y)
      for (i in seq_along(facet_marker_non)) {
        facet_axis <- names(facet_marker_non)[i]
        plot_tbl <- plot_tbl[plot_tbl[[facet_marker_non[i]]] == level_vec[facet_axis], ]
      }
      plot_cyto(
        data = plot_tbl,
        marker = marker_vec,
        lab = lab,
        limits_expand = NULL,
        limits_equal = limits_equal_within_plot,
        font_size = font_size
      )
    })
  })







  out_list
}
