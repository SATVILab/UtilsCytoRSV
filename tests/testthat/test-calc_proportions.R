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

test_that("calc_freq warns for out of range values", {
  # Test frequencies above 100
  mock_data_high <- tibble::tibble(
    den = c(10, 10),
    num = c(15, 20)
  )
  expect_warning(
    calc_freq(mock_data_high, den = "den", num = "num"),
    "frequencies above 100 observed"
  )

  # Test frequencies below 0
  mock_data_low <- tibble::tibble(
    den = c(10, 10),
    num = c(-5, 1)
  )
  expect_warning(
    calc_freq(mock_data_low, den = "den", num = "num"),
    "frequencies below 0 observed"
  )

  # Test remove_counts parameter
  mock_data <- tibble::tibble(
    pop = "cd3",
    "cd4" = 20:21,
    "ifng" = 2:3
  )
  result <- calc_freq(mock_data, den = "cd4", num = "ifng", remove_counts = TRUE)
  expect_false("cd4" %in% colnames(result))
  expect_false("ifng" %in% colnames(result))
})

test_that("calc_prop warns for out of range values", {
  # Test proportions above 1
  mock_data_high <- tibble::tibble(
    den = c(10, 10),
    num = c(15, 20)
  )
  expect_warning(
    calc_prop(mock_data_high, den = "den", num = "num"),
    "frequencies above 100 observed"
  )

  # Test proportions below 0
  mock_data_low <- tibble::tibble(
    den = c(10, 10),
    num = c(-5, 1)
  )
  expect_warning(
    calc_prop(mock_data_low, den = "den", num = "num"),
    "frequencies below 0 observed"
  )

  # Test remove_counts parameter
  mock_data <- tibble::tibble(
    pop = "cd3",
    "cd4" = 20:21,
    "ifng" = 2:3
  )
  result <- calc_prop(mock_data, den = "cd4", num = "ifng", remove_counts = TRUE)
  expect_false("cd4" %in% colnames(result))
  expect_false("ifng" %in% colnames(result))
})
