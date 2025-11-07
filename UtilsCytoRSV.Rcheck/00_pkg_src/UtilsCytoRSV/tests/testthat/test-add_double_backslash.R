test_that("add_double_backslash works", {
  expect_identical(
    add_double_backslash("anc"),
    "anc"
  )
  expect_identical(
    add_double_backslash("anc+"),
    "anc\\+"
  )
  expect_identical(
    add_double_backslash("anc++"),
    "anc\\+\\+"
  )
  expect_identical(
    add_double_backslash("a-nc++"),
    "a\\-nc\\+\\+"
  )
})
