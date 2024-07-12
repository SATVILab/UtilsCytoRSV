get_facet_levels <- function(data, marker, facet) {
  marker_vec <- marker |>
    unlist() |>
    unique()
  facet_vec <- switch(as.character(!is.null(facet)),
    "TRUE" = setNames(facet, NULL),
    "FALSE" = NULL
  )
  cn_vec_plot <- c(facet_vec[!facet_vec == "marker"], marker_vec)
  data <- data[, cn_vec_plot]

  facet_levels_list <- list()

  if (is.null(facet)) {
    facet <- c(x = NA, y = "marker")
  }

  facet_Var_to_var_lab_vec <- NULL
  facet_axis_to_Var_lab_vec <- c("x" = "Var1", "y" = "Var2")
  if ("marker" %in% facet) {
    facet_marker_axis <- names(facet)[facet == "marker"]
    facet_Var_to_var_lab_vec <- facet_Var_to_var_lab_vec |>
      c("marker" |> setNames(facet_axis_to_Var_lab_vec[facet_marker_axis]))
    facet_levels_list[[facet_marker_axis]] <- purrr::map_chr(
      .x = marker,
      .f = function(x) paste0(x, collapse = "~~~")
    )
    facet_marker_non <- facet[-which(names(facet) == facet_marker_axis)]
  } else {
    facet_marker_non <- facet
  }

  for (i in seq_along(facet_marker_non)) {
    facet_axis <- names(facet_marker_non)[i]
    if (is.na(facet_marker_non[[i]])) {
      facet_levels_list[[facet_axis]] <- NULL
    } else {
      facet_levels_list[[facet_axis]] <- unique(data[[facet_marker_non[[i]]]])
      facet_Var_to_var_lab_vec <- facet_Var_to_var_lab_vec |>
        c(facet_marker_non[i] |> setNames(facet_axis_to_Var_lab_vec[facet_axis]))
    }
  }
  facet_level_combn_tbl <- expand.grid(
    facet_levels_list[["x"]],
    facet_levels_list[["y"]]
  )
  names(facet_level_combn_tbl) <- facet_Var_to_var_lab_vec[names(facet_level_combn_tbl)]

  # first column specifies variable that goes specifies x-variable
  # for a given column, and second column specifies variable that
  # specifies y-value for a given row
  facet_level_combn_tbl <- facet_level_combn_tbl[, c(facet["x"], facet["y"])] |>
    dplyr::mutate_all(as.character)
}
