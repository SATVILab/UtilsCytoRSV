#' @title Calculate summed proportion/frequencies
#'
#' @param .data dataframe. Each row pertains to one.
#' @param grp character vector.
#' Names of columns in \code{.data} that together
#' define individual groups within which responses must be summed
#' (for example, participant ID and stim).
#' If \code{NULL}, then no grouping is done.
#' Default is \code{NULL}.
#' @param cmbn character.
#' Column specifying marker/channel combination.
#' @param levels character vector of length 2.
#' Indicators for the expression
#' and non-expression.
#' Default is \code{c("+", "-")}.
#' @param markers_to_sum,markers_to_keep character vector.
#' Markers to sum over/markers to not keep. Specify
#' either one. If both are specified, then markers_to_sum
#' is used and markers_to_keep is ignored.
#' Default is \code{NULL}.
#' Note that only \code{markers_to_sum} is
#' implemented thus far.
#' @param markers_to_keep character vector.
#' Markers to not sum over.
#' Overridden by \code{markers_to_sum},
#' if that is not \code{NULL}.
#' Default for \code{markers_to_keep} is
#' \code{NULL}.
#' Note that \code{markers_to_keep} is not
#' implemented yet.
#' @param resp character vector.
#' Names of columns to sum over
#' (may specify more than one, e.g. unstim and stim columns).
#' @param out_of_range numeric vector of length 2.
#' specifies values that are considered out of range.
#' Nothing happens as yet if values are out of range - still
#' needs to be implemented.
#'
#' @details
#' If cytokine combinations are expressed in COMPASS format
#' (in other words separate cytokines by & and specify
#' non-expression by !),
#' then use `compassutils::convert_cyt_combn_format` to
#' convert to "standard format", e.g. "IFNg+IL2+".
#'
#' @examples
#' data("data_count")
#' data_test <- data_count %>%
#'   calc_prop(
#'     count_den = "count_pop_den",
#'     count_num = "count_pop_num"
#'   ) %>%
#'   dplyr::select(-c(count_pop_den, count_pop_num)) %>%
#'   dplyr::arrange(SubjectID, VisitType, stim, cyt_combn)
#'
#' data_out <- sum_over_markers(
#'   .data = data_test,
#'   grp = c("SubjectID", "VisitType", "stim"),
#'   cmbn = "cyt_combn",
#'   markers_to_sum = c("IFNg", "IL2", "IL17"),
#'   levels = c("-", "+"),
#'   resp = "prop"
#' )
#' @export
sum_over_markers <- function(.data,
                             grp = NULL,
                             cmbn,
                             levels = c("+", "-"),
                             markers_to_sum = NULL,
                             markers_to_keep = NULL,
                             resp,
                             out_of_range = c(0, 1)) {

  # keep so that order of columns is
  # the same at the end as at the start
  cn_vec_final <- colnames(.data)
  cn_vec_final <- cn_vec_final[cn_vec_final %in% c(resp, cmbn, grp)]

  # remove markers to be summed over, so
  # we can sum within remaining levels
  .data[[cmbn]] <- remove_markers(.data[[cmbn]],
    markers = markers_to_sum,
    levels = levels
  )

  .data <- .data %>%
    dplyr::group_by_at(c(grp, cmbn))

  .data <- .data %>%
    dplyr::summarise_at(resp, sum)

  .data <- .data %>%
    dplyr::ungroup()

  # checks

  .data[, cn_vec_final]
}
