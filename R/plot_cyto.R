#' @title Plot cytometry expression data
#'
#' @description Plot a hex-plot with suitable default for a single
#' sample for cytometry (CyTOF, flow) data.
#'
#' @param data dataframe. Columns are markers (or channels - it doesn't matter,
#' as long as you know which), and rows are cells.
#' @param marker character vector. Columns of \code{data} to plot. The first
#' element is plotted on the x-axis.
#' @param lab named character vector. If not \code{NULL}, then
#' the axis titles for marker are selected using it.
#' @param limits_expand list. If not \code{NULL},
#' then it is (effectively) passed onto \code{ggplot2::limits_expand} to
#' ensure that certain values are included in the plot (such as, for example, 0
#' if that is the minimum value possible but it may not be plotted). If not named, then
#' must consist of one numeric vector that will then force all values in the numeric value
#' to be included in the plot. If named, then must have names \code{x} and/or \code{y},
#' with the elements again being numeric vectors that must be included in plot.
#' @param limits_equal logical. If \code{TRUE}, then the ranges on the x- and y-axes
#' must be equal. Effectively applied after expand_grid is applied. Default is \code{FALSE}.
#' @param font_size integer. Font size to be passed on to
#' \code{cowplot::theme_cowplot(font_size = <font_size>)}.
#' @param coord_equal logical. If \code{TRUE},
#' then the \code{coord_equal} ggplot2 function is applied to
#' the plot, making units take up the same visual space on the x-
#' and y-axes.
#' Note that this will cause plots to note be able to be aligned
#' using functions like \code{cowplot::plot_grid} and 
#' \code{patchwork::align_plots}.
#' Default is \code{TRUE}.
#' @param ... arguments passed to \code{ggplot2::geom_hex}.
#'
#' @import ggplot2
#'
#' @details Uses the following defaults:
#' \itemize{
#'    \item Plots a hex plot if \code{marker} param has length 2
#'    \item Uses theme \code{cowplot::theme_cowplot} with background grid
#'    \code{cowplot::background_grid(major = 'xy')}.
#'    \item If a hex plot, then fill colour is defined by
#'    \code{scale_fill_viridis_c} with params \code{trans = 'log10'}.
#'    \item axis units are equal between the axes
#' }
#'
#' @export
#'
#' @examples
#'
#' data("GvHD", package = "flowCore")
#' ex_tbl <- flowCore::exprs(GvHD[[1]])
#' marker <- c("FL2-H", "FL3-H")
#' plot_cyto(
#'   data = ex_tbl,
#'   marker = marker
#' )
#' lab_vec <- cytoutils::chnl_lab(data = GvHD)
#' plot_cyto(
#'   data = ex_tbl,
#'   marker = marker,
#'   lab = lab_vec
#' )
plot_cyto <- function(data, marker, lab = NULL,
                      coord_equal = TRUE,
                      limits_expand = NULL, limits_equal = FALSE,
                      font_size = 14, ...) {
  # checks
  # -----------------

  if (!is.data.frame(data)) stop("data must be a dataframe")
  if (!is.null(lab)) {
    if (!is.character(lab) || is.null(names(lab))) {
      stop("lab must be a named character vector (if not NULL)")
    }
  }
  if (!is.numeric(font_size)) {
    stop("font_size must be numeric")
  }

  # prep
  # --------------------

  # plot_tbl
  plot_tbl <- data[, marker]
  colnames(plot_tbl) <- c("V1", "V2")
  # axis labels
  if (!is.null(lab)) {
    marker <- lab[marker]
  }
p
  # base plot
  p <- ggplot(
    plot_tbl,
    aes(x = V1, y = V2)
  ) +
    cowplot::theme_cowplot(font_size) +
    geom_hex(...) +
    scale_fill_viridis_c(
      trans = "log10",
      name = "Count"
    ) +
    cowplot::background_grid(major = "xy") +
    labs(x = marker[1], y = marker[2])

  if (coord_equal) p <- p + coord_equal()

  # return now if axis_limits fn not required
  if (is.null(limits_expand) && !limits_equal) {
    return(p)
  }

  if (!requireNamespace("ggutils", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("SATVILab/ggutils")
  }

  ggutils::axis_limits(
    p = p,
    limits_expand = limits_expand,
    limits_equal = limits_equal
  )
}
