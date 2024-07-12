#' @title Subtract background
#'
#' @description Subtract the unstim measurement from one or more columns.
#'
#' @param .data dataframe. Contains columns for subtraction.
#' @param grp character vector. Columns in \code{.data} to group by.
#' @param stim character. Column in \code{.data} specifying stimulation.
#' @param resp character vector. Column(s) in \code{data} to subtract background from.
#' @param uns character. String in \code{stim} that indicates the unstim condition.
#' @param remove_uns logical. If \code{TRUE}, then the unstim rows are removed.
#'
#' @return A dataframe.
#'
#' @examples
#' .data_test <- data.frame(
#'   pid = rep(c("a", "b"), each = 3),
#'   stim = c("mtb", "ebv", "uns") |>
#'     c("uns", "ebv", "mtb"),
#'   resp1 = 1:6,
#'   resp2 = 17:12 * 2
#' )
#' data_out <- subtract_background(
#'   .data = .data_test,
#'   grp = "pid",
#'   stim = "stim",
#'   uns = "uns",
#'   resp = c("resp1", "resp2"),
#'   remove_uns = FALSE
#' )
#' @export
subtract_background <- function(.data, grp = NULL, stim, resp, uns,
                                remove_uns = TRUE) {
  # keep so that order of columns is
  # the same at the end as at the start
  cn_vec_final <- colnames(.data)
  cn_vec_final <- cn_vec_final[cn_vec_final %in% c(resp, stim, grp)]

  .data <- .data |>
    dplyr::group_by_at(grp)

  check_uns_per_group <- .data |>
    dplyr::summarise(n_stim = sum(.data[[stim]] == uns)) |>
    dplyr::ungroup() |>
    dplyr::summarise(d1 = sum(n_stim != 1) > 1) |>
    dplyr::pull(d1)
  if (check_uns_per_group) {
    stop("not every group has one unstim measurement exactly")
  }

  for (resp_curr in resp) {
    print(resp_curr)
    .data <- .data |>
      dplyr::mutate(resp_uns = .data[[resp_curr]][.data[[stim]] == uns])
    .data[[resp_curr]] <- .data[[resp_curr]] - .data$resp_uns
    .data <- .data[, -which(colnames(.data) == "resp_uns")]
  }


  if (remove_uns) .data <- .data[!.data[[stim]] == uns, ]

  .data |>
    dplyr::ungroup()
}
