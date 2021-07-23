#' @title Calculate summed proportion/frequencies
#'
#' @param .data dataframe. Each row pertains to one.
#' @param grp character vector.
#' Names of columns in \code{.data} that together
#' define individual groups within which responses must be summed
#' (for example, participant ID and stim).
#' If \code{NULL}, then no grouping is done.
#' Default is \code{NULL}.
#' @param marker character.
#' Column specifying marker combination.
#' @param ind_pos,ind_neg character.
#' Indicators for the expression
#' and non-expression, respectively.
#' Defaults are "+" and "-"
#' @param markers_to_sum,markers_to_keep character vector.
#' Markers to sum over/markers to not keep. Specify
#' either one. If both are specified, then markers_to_sum
#' is used and markers_to_keep is ignored.
#' Default is \code{NULL}.
#' @param markers_to_keep character vector.
#' Markers to not sum over.
#' Overridden by \code{markers_to_sum},
#' if that is not \code{NULL}.
#' Default for \code{markers_to_keep} is
#' \code{NULL}.
#'
#' @details
#' If cytokine combinations are expressed in COMPASS format
#' (in other words separate cytokines by & and specify
#' non-expression by !),
#' then use `compassutils::convert_cyt_combn_format` to
#' convert to "standard format", e.g. "IFNg+IL2+".
#' @importFrom rlang !! !!! ensym
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
                                  levels = levels)

  .data <- .data %>%
    dplyr::group_by_at(c(grp, cmbn))

  .data <- .data %>%
    dplyr::summarise(resp2 = sum(.data[[resp]]),
                     .groups = "drop")
  .data[[resp]] <- .data$resp2
  .data <- .data[,-which(colnames(.data) == "resp2")]


  # checks

  .data[,cn_vec_final]

}
