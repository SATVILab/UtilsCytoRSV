test_that("removing marker levels works", {
  levels <- c("+", "-")
  cmbn_orig <- data_count$cyt_combn[1:5] |>
    setNames(NULL)
  cmbn <- cmbn_orig
  markers <- c("IFNg", "IL17")
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = c("IFNg", "TNF", "IL17"),
      levels = c("-", "+")
    ),
    c("IL2-", "IL2-", "IL2-", "IL2+", "IL2+")
  )
  cmbn <- gsub("\\+", "p", cmbn)
  cmbn <- gsub("\\-", "n", cmbn)
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = c("IFNg"),
      levels = c("n", "p")
    ),
    c(
      "IL2nTNFnIL17p", "IL2nTNFpIL17n",
      "IL2nTNFpIL17p", "IL2pTNFnIL17n",
      "IL2pTNFnIL17p"
    )
  )
  cmbn <- gsub("p|\\+", "~2~", cmbn)
  cmbn <- gsub("n|\\-", "~1~", cmbn)
  levels <- c("~1~", "~2~")
  markers <- "IFNg"
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = c("IFNg"),
      levels = c("~1~", "~2~")
    ),
    c(
      "IL2~1~TNF~1~IL17~2~", "IL2~1~TNF~2~IL17~1~", "IL2~1~TNF~2~IL17~2~",
      "IL2~2~TNF~1~IL17~1~", "IL2~2~TNF~1~IL17~2~"
    )
  )
  cmbn <- gsub("~2~", "+", cmbn)
  cmbn <- gsub("~1~", "-", cmbn)
  cmbn <- gsub("IFNg", "IFNg-beads", cmbn)
  levels <- c("+", "-")
  markers <- "IFNg"
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = "IFNg-beads",
      levels = levels
    ),
    c(
      "IL2-TNF-IL17+", "IL2-TNF+IL17-",
      "IL2-TNF+IL17+", "IL2+TNF-IL17-",
      "IL2+TNF-IL17+"
    )
  )
  cmbn <- gsub("IL2", "-2349d=", cmbn)
  levels <- c("+", "-")
  markers <- "IFNg"
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = c("IFNg-beads"),
      levels = levels
    ),
    c(
      "-2349d=-TNF-IL17+", "-2349d=-TNF+IL17-", "-2349d=-TNF+IL17+",
      "-2349d=+TNF-IL17-", "-2349d=+TNF-IL17+"
    )
  )
  expect_identical(
    remove_markers(
      cmbn = cmbn,
      markers = c("IFNg-beads", "-2349d="),
      levels = levels
    ),
    c(
      "TNF-IL17+", "TNF+IL17-", "TNF+IL17+",
      "TNF-IL17-", "TNF-IL17+"
    )
  )
})
