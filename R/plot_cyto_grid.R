#' @title Plot cytometry data ggplot2::facet_grid-style
#'
#' @inheritParams plot_cyto
#' @param limits_equal_within 'grid', 'facet_x', 'facet_y', 'plot'. Specifies
#' precisely which plots are to be
#'
#' @export
#'
#' @return list with named elements 'plots' and/or 'grid'.
plot_cyto_grid <- function(data, marker, lab = NULL,
                           facet = NULL, n_col = NULL,
                           limits_expand = NULL, limits_equal = FALSE,
                           limits_equal_within = 'facet',
                           font_size = 14, return_plots = FALSE,
                           return_grid = TRUE){

  if(is.null(facet)){
    p <- purrr::map(marker, function(mk){
      plot_cyto_grid(
        data = data,
        marker = marker,
        lab = lab,
        limits_expand = limits_expand,
        limits_equal = limits_equal,
        font_size = font_size
      )
    })
  }

}
