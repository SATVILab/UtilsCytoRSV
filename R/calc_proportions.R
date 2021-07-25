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
#' @param freq,pprop character. Name of output column.
#' @param remove_counts logical. If \code{TRUE}, then
#' \code{num} and \code{den} columns are removed
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
#'   den = "cd4",
#'   num = "ifng")
#' calc_prop(
#'   .data = mock_data,
#'   den = "cd4",
#'   num = "ifng")
#'  @export
calc_freq <- function(.data, den, num,
                      freq = "freq",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data[,freq] <- .data[[num]] / .data[[den]] * 1e2
  if (remove_counts) {
    .data <- .data[,-which(colnames(.data) %in% c(den, num))]
  }
  if (warn) {
    if (any(.data[[freq]] > 100)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data[[freq]] < 0)) {
      warning("frequencies below 0 observed")
    }
  }
  .data
}

#' @rdname calc_proportions
#' @export
calc_prop <- function(.data, den, num,
                      prop = "prop",
                      remove_counts = FALSE,
                      warn = TRUE) {
  .data <- calc_freq(
    .data = .data,
    den = den,
    num = num,
    freq = "aweadsfajfk"
    ) %>%
    dplyr::mutate(aweadsfajfk = aweadsfajfk/1e2)
  colnames(.data)[which(colnames(.data) == "aweadsfajfk")] <- prop

  if (remove_counts) {
    .data <- .data[,-which(colnames(.data) %in% c(den, num))]
  }
  if (warn) {
    if (any(.data[[prop]] > 1)) {
      warning("frequencies above 100 observed")
    }
    if (any(.data[[prop]] < 0)) {
      warning("frequencies below 0 observed")
    }
  }
  .data
}
