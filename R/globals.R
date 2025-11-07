#' @keywords internal
#' @importFrom stats setNames ecdf
#' @import ggplot2
NULL

# Global variables used in package
utils::globalVariables(c(
  "V1", "V2", "SubjectID", "stim", "n_cell_stim", "count_stim",
  "VisitType", "cyt_combn", "count_pop_den", "count_pop_num",
  "aweadsfajfk", "limits_equal_cross", "limits_equal", "limits_equal_within",
  "facet", "plot_tbl", "limits_expand", "mk", "x", "ecdf",
  "facet_level_combn_tbl", "out_list", "markers_to_sum", "marker",
  "ind", "ind_to_orig", "n_stim", "d1"
))
