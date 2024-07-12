#' @title Add a double backslash before punct characters
#'
#' @description Useful for matching.
#'
#' @param string character. String to transform.
#'
#' @return A string.
add_double_backslash <- function(string) {
  len <- nchar(string)
  vapply(seq_len(len), function(i) {
    chr <- substr(string, i, i)
    ifelse(grepl("[[:punct:]]", chr), paste0("\\", chr), chr)
  }, "") |>
    paste0(collapse = "")
}
