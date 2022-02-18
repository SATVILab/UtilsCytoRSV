#' @title Get empirical CDF for each column in a dataframe
#'
#' @description Get the empirical cumulative distribution
#' function for each column in a dataframe.
#'
#' @param expr_tbl dataframe. Each row corresponds to an observation,
#' and each column a variable. In the case of cytometry data,
#' the rows are cells and the columns are marker expression.
#'
#' @return A named list. Each element corresponds to a column in
#' \code{expr_tbl}. Its name is the name of that column,
#' and its content is the empirical cdf of that column.
#'
#' @examples
#' expr_tbl <- tibble::tibble(
#'   v1 = rnorm(20),
#'   v2 = rnorm(20, mean = 1)
#' )
#' get_ecdf_list(expr_tbl)
get_ecdf_list <- function(expr_tbl) {

  # checks
  # -------------------
  if (!"data.frame" %in% class(expr_tbl)) {
    stop("expr_tbl must have be a dataframe (or tibble or data.frame")
  }
  if (is.null(colnames(expr_tbl))) stop("expr_tbl must have names")

  if (!all(purrr::map_lgl(expr_tbl, is.numeric))) {
    stop("each column must be numeric")
  }

  purrr::map(expr_tbl, ecdf) %>%
    setNames(colnames(expr_tbl))
}
