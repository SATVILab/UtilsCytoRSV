test_that("calc proportions work", {
  mock_data <- tibble::tibble(
    pop = "cd3",
    "cd4" = 20:21,
    "ifng" = 2:3
  )
  expect_identical(
    mock_data |>
      dplyr::mutate(freq = 2:3 / 20:21 * 1e2),
    calc_freq(mock_data, den = "cd4", num = "ifng")
  )
  expect_identical(
    mock_data |>
      dplyr::mutate(prop = 2:3 / 20:21),
    calc_prop(mock_data, den = "cd4", num = "ifng")
  )
  expect_identical(
    mock_data |>
      dplyr::mutate(prop = 2:3 / 20:21),
    calc_prop(mock_data, den = "cd4", num = "ifng")
  )
})
