#' @title Create test and example data
#'
#' @description Creates \code{data/data_count.rda}. The \code{tests} dataset is
#' smaller.
#'
#' @return Invisibly returns the paths to the datasets
#' created.
.save_mock_data <- function() {
  if (!requireNamespace("usethis", quietly = TRUE)) {
    install.packages("usethis")
  }
  .install_pkg_cran("here")

  path_save_vec <- NULL
  if (!"DESCRIPTION" %in% list.files(here::here())) {
    stop("Not saving data as not in a package")
  }
  data_count <- DataTidyACSCyTOFCytokinesTCells::cd4_th1_il17$stats_combn_tbl %>%
    dplyr::filter(SubjectID == "010782") %>%
    dplyr::filter(stim %in% c("mtb", "ebv")) %>%
    calc_freq("n_cell_stim", "count_stim") %>%
    dplyr::rename(
      count_pop_den = n_cell_stim,
      count_pop_num = count_stim
    ) %>%
    dplyr::select(
      SubjectID, VisitType, stim,
      cyt_combn, count_pop_den, count_pop_num
    ) %>%
    dplyr::mutate(
      cyt_combn = UtilsCompassSV::convert_cyt_combn_format(
        cyt_combn,
        to = "std"
      )
    )

  usethis::use_data(data_count, overwrite = TRUE)
  path_save <- file.path(here::here(), "data", "data_count.rda")

  path_save_vec <- c(path_save_vec, path_save)

  unique(path_save)
}
