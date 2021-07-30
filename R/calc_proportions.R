
#' @rdname calc_proportions
#' @title Calculate frequencies and proportions
#'
#' @description Calculate frequencies and proprtions, and make
#' warnings if out-of-range values are observed.
#'
#' @param .data dataframe. Has column in long format, i.e. one row corresponds to
#' one cell population for one sample.
#' @param num,den character. Names of columns giving counts for
#' numerator and denominator, respectively.
#' @param nm character. Name of output column.
#' @param remove_counts logical. If \code{TRUE}, then
#' \code{num} and \code{den} columns are removed
#' after calculation. Default is \code{FALSE}.
#' @param warn logical. If \code{TRUE}, then warnings
#' are printed if non-NA frequencies (proportions)
#' above 100 (1) or less than 0
#' are observed. Default is \code{TRUE}.
#'
#' @return A dataframe, with new column \code{freq} or \code{prop}.
#' @export
#' @examples
#' library(cytoutils)
#' mock_data <- tibble::tibble(
#'   pop = "cd4",
#'   "cd4" = rnorm(10, mean = 2000, sd = 100),
#'   "ifng" = rnorm(10, mean = 500, sd = 20)
#'   )
#' calc_freq(
#'   .data = mock_data,
#'   den = "cd4",
#'   num = "ifng")
#' calc_prop(
#'   .data = mock_data,
#'   den = "cd4",
#'   num = "ifng")
calc_freq <- function(.data, den, num,
                      nm = "freq",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data[,nm] <- .data[[num]] / .data[[den]] * 1e2
  if (remove_counts) {
    .data <- .data[,-which(colnames(.data) %in% c(den, num))]
  }
  if (warn) {
    if (any(.data[[nm]] > 100, na.rm = TRUE)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data[[nm]] < 0, na.rm = TRUE)) {
      warning("frequencies below 0 observed")
    }
  }
  .data
}

#' @rdname calc_proportions
#' @export
calc_prop <- function(.data, den, num,
                      nm = "prop",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data <- calc_freq(
    .data = .data,
    den = den,
    num = num,
    nm = "aweadsfajfk"
    ) %>%
    dplyr::mutate(aweadsfajfk = aweadsfajfk/1e2)
  colnames(.data)[which(colnames(.data) == "aweadsfajfk")] <- nm

  if (remove_counts) {
    .data <- .data[,-which(colnames(.data) %in% c(den, num))]
  }
  if (warn) {
    if (any(.data[[nm]] > 1, na.rm = TRUE)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data[[nm]] < 0, na.rm = TRUE)) {
      warning("frequencies below 0 observed")
    }
  }
  .data
}
