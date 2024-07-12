#' @title Sum over markers
#' @param cmbn character vector. Each element is a strng containing
#' levels of a given marker.
#' @param levels character vector. Levels of the marker, e.g. c("+", "-").
#'
#' @return Character vector with specified markers (and corresponding
#' level indicators) removed.
#'
#' @details Still need to write remove_other_markers function.
#'
#' @seealso retain_marker_levels
remove_markers <- function(cmbn, markers, levels) {
  if (is.null(markers)) {
    return(rep("", length(cmbn)))
  }
  levels <- unique(levels)
  levels_orig <- levels
  levels <- vapply(levels_orig, add_double_backslash, "")
  # map between orig and transformed
  # so that it can be correctly displayed
  # in output after matching
  levels_to_orig <- setNames(levels_orig, levels)
  n_lvl <- length(levels)


  markers <- unique(markers)
  markers_orig <- markers
  markers <- vapply(markers, add_double_backslash, "")
  markers_to_orig <- setNames(markers_orig, markers)
  for (mk in markers) {
    match_string_end <- paste0(mk, levels)
    match_string_at_start <- paste0("^", match_string_end, collapse = "|")
    cmbn <- gsub(match_string_at_start, "", cmbn)
    for (lvl in levels) {
      match_string_in_mid <- paste0(lvl, match_string_end, collapse = "|")
      cmbn <- gsub(match_string_in_mid, levels_to_orig[lvl], cmbn)
    }
  }
  cmbn
}

remove_other_markers <- function(cmbn, markers, levels) {
  if (!is.null(markers_to_sum)) {

  } else if (!is.null(markers_to_keep)) {
    markers_to_keep <- unique(markers_to_keep)
    markers_to_keep <- purrr::map_chr(
      markers_to_keep,
      add_double_backslash
    )
    orig_order <- .data[[marker]][1] %>%
      stringr::str_locate(markers_to_keep)
    markers_to_keep <- markers_to_keep[order(orig_order[, "start"])]
    markers_to_keep
    mk_vec_rep <- rep("", nrow(.data))
    for (mk in markers_to_keep) {
      for (ind_level in ind) {
        mk_level_add <- paste0(mk, ind_to_orig[ind_level])
        mk_level_match <- paste0(mk, ind_level)
        match_string_start <- paste0("^", mk_level_match)
        match_string_mid <- paste0(ind, mk_level_match,
          collapse = "|"
        )
        match_string <- paste0(
          match_string_start,
          match_string_mid,
          collapse = "|"
        )
        match_vec_ind <- grepl(
          match_string,
          .data[[marker]]
        ) + 1
        mk_vec_rep <- paste0(
          mk_vec_rep,
          c("", mk_level_add)[match_vec_ind]
        )
      }
    }
  }
}
