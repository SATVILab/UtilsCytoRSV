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
#' @param expand_grid list. If not \code{NULL},
#' then it is (effectively) passed onto \code{ggplot2::expand_limits} to
#' ensure that certain values are included in the plot (such as, for example, 0
#' if that is the minimum value possible but it may not be plotted). If not named, then
#' must consist of one numeric vector that will then force all values in the numeric value
#' to be included in the plot. If named, then must have names \code{x} and/or \code{y},
#' with the elements again being numeric vectors that must be included in plot.
#' @param axis_range_equal logical. If \code{TRUE}, then the ranges on the x- and y-axes
#' must be equal. Effectively applied after expand_grid is applied. Default is \code{FALSE}.
#' @param font_size integer. Font size to be passed on to
#' \code{cowplot::theme_cowplot(font_size = <font_size>)}.
#'
#' @import ggplot2
#'
#'
#' @details Uses the following defaults:
#' \itemize{
#'    \item Plots a hex plot if \code{marker} param has length 2
#'    \item Uses theme \code{cowplot::theme_cowplot} with background grid
#'    \code{cowplot::background_grid(major = 'xy')}.
#'    \item If a hex plot, then fill colour is defined by
#'    \code{scale_fill_viridis_c} with params \code{trans = 'log10'}.
#' }
plot_cyto <- function(data, marker, lab = NULL,
                      expand_limits = NULL, axis_range_equal = FALSE,
                      font_size = 14){
  # checks
  # -----------------

  if(!is.data.frame(data)) stop("data must be a dataframe")
  if(!is.null(lab)){
    if(!is.character(lab) || is.null(names(lab))){
      stop("lab must be a named character vector (if not NULL)")
    }
  }
  if(!is.null(expand_limits)){
    if(!is.list(expand_limits)){
      stop("expand_limits must be a list (if not NULL)")
    }
    if(length(expand_limits) == 2 && is.null(names(expand_limits))){
      stop("expand_limits must be named if of length 2")
    }
    if(length(expand_limits) > 2){
      stop("expand_limits must have length 1 or 2 (if not NULL)")
    }
    if(!is.null(names(expand_limits))){
      if(length(setdiff(names(expand_limits), c('x', 'y'))) > 0){
        stop("expand_limits must have names of 'x' and/or 'y' (if named)")
      }
    }
    class_input <- purrr::map_lgl(expand_limits, is.numeric) %>% all()
    if(!class_input){
      stop("input to expand_limits must be numeric (if expand_limits not NULL)")
    }
  }
  if(!is.logical(axis_range_equal)){
    stop("axis_range_equal must be logical, if not NULL")
  }
  if(!is.numeric(font_size)){
    stop("font_size must be numeric")
  }

  # prep
  # --------------------

  # plot_tbl
  plot_tbl <- data[,marker]
  colnames(plot_tbl) <- c("V1", "V2")
  # axis labels
  if(!is.null(lab)){
    marker <- lab[marker]
  }

  # axis limits
  p <- ggplot(plot_tbl,
              aes(x = V1, y = V2)) +
    cowplot::theme_cowplot(font_size) +
    geom_hex() +
    scale_fill_viridis_c(trans = 'log10',
                         name = 'Count') +
    cowplot::background_grid(major = 'xy') +
    labs(x = marker[1], y = marker[2]) +
    coord_equal()

  if(!is.null(expand_limits) || axis_range_equal){
    # put into list(x = ..., y = ...) form
    # if not already

    # calc ranges in advance if needed
    # --------------------
    if(axis_range_equal){
      range_x <- range(plot_tbl$V1)
      range_y <- range(plot_tbl$V1)
      range <- c(min(range_x[1], range_y[1]),
                 c(max(range_x[2], range_y[2])))
    }

    # tidy expand_limits if provided
    # ------------------

    # ensure that expand_limits is named if
    # it's specified
    if(!is.null(expand_limits)){
      if(is.null(names(expand_limits))){
        expand_limits <- list(
          x = expand_limits[[1]],
          y = expand_limits[[1]]
        )
      }
      # ensure that expand_limits consists of
      # two sorted (not strictly) variables
      for(i in seq_along(expand_limits)){
        expand_limits[[i]] <- c(min(expand_limits[[i]]),
                                max(expand_limits[[i]]))
      }
    }

    # put expand_limits together with axis_range_equal,
    # if provided
    if(is.null(expand_limits)){
      # we know now that axis_range_equal is true
      expand_limits <- list(
        x = range,
        y = range
      )
    } else{
      # axis_range_equal may or may not be true
      if(axis_range_equal){
        for(i in seq_along(expand_limits)){
          expand_limits[[i]] <- c(min(range, expand_limits[[i]]),
                                  max(range, expand_limits[[i]]))
        }
        if(length(expand_limits) == 1){
          nm <- setdiff(c('x', 'y'), names(expand_limits))
          expand_limits %<>%
            append(list(range) %>% setNames(nm))
        }
      }
    }

    expand_limits_arg <- purrr::map_chr(seq_along(expand_limits), function(i){
      vals <- paste0(expand_limits[[i]], collapse = ", ")
        paste0(names(expand_limits)[i], " = c(", vals, ")")
      }) %>%
      paste0(collapse = ", ")

    parse_text <- paste0("p <- p + expand_limits(", expand_limits_arg, ")")
    env <- environment()
    eval(parse(text = parse_text), envir = env)
  }
  p
}

