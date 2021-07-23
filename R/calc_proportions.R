#' @rdname calc_proportions
#' @title Calculate frequencies and proportions
#'
#' @description Calculate frequencies and proprtions, and make
#' warnings if out-of-range values are observed.
#'
#' @param .data dataframe. Has column in long format, i.e. one row corresponds to
#' one cell population for one sample.
#' @param count_num,count_den character. Names of columns giving counts for
#' numerator and denominator, respectively.
#' @param name_col_out character. Name of output column.
#' If \code{calc_freq} is used, then its default is \code{freq}.
#' If \code{calc_prop} is used, then its default is \code{prop}.
#' @param remove_counts logical. If \code{TRUE}, then
#' \code{count_num} and \code{count_den} columns are removed
#' after calculation. Default is \code{FALSE}.
#' @param warn logical. If \code{TRUE}, then warnings
#' are printed if frequencies (proportions)
#' above 100 (1) or less than 0
#' are observed. Default is \code{TRUE}.
#'
#' @return A dataframe, with new column \code{freq} or \code{prop}.
#'
#' @examples
#' library(cytoutils)
#' mock_data <- tibble::tibble(
#'   pop = "cd4",
#'   "cd4" = rnorm(10, mean = 2000, sd = 100),
#'   "ifng" = rnorm(10, mean = 500, sd = 20)
#'   )
#' calc_freq(
#'   .data = mock_data,
#'   count_den = "cd4",
#'   count_num = "ifng")
#' calc_prop(
#'   .data = mock_data,
#'   count_den = "cd4",
#'   count_num = "ifng")
#'  @export
calc_freq <- function(.data, count_den, count_num,
                      name_col_out = "freq",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data_out <- .data
  .data_out[,name_col_out] <- .data[[count_num]] / .data[[count_den]] * 1e2
  if (remove_counts) {
    .data_out <- .data_out[,-c(count_den, count_num)]
  }
  if (warn) {
    if (any(.data_out[[name_col_out]] > 100)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data_out[[name_col_out]] < 0)) {
      warning("frequencies below 0 observed")
    }
  }
  .data_out
}

#' @rdname calc_proportions
#' @export
calc_prop <- function(.data, count_den, count_num,
                      name_col_out = "prop",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data_out <- calc_freq(
    .data = .data,
    count_den = count_den,
    count_num = count_num,
    name_col_out = "zdk218312"
    ) %>%
    dplyr::mutate(zdk218312 = zdk218312/1e2)
  colnames(.data_out)[ncol(.data_out)] <- name_col_out

  if (remove_counts) {
    .data_out <- .data_out[,-c(count_den, count_num)]
  }
  if (warn) {
    if (any(.data_out[[name_col_out]] > 1e2)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data_out[[name_col_out]] < 0)) {
      warning("frequencies below 0 observed")
    }
  }
  .data_out
}

subtract_background <- function(.data, col_grp, col_stim, nm_uns) {

}
