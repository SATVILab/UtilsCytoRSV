.assert_string <- function(x) {
  missing(x) || is.null(x) ||
    !all(is.character(x)) || !length(x == 1L) || !nchar(x) > 0L
}
