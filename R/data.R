#' Example cytometry count data
#'
#' A dataset containing cytokine combination counts from cytometry data
#' for demonstration and testing purposes.
#'
#' @format A tibble with 64 rows and 6 variables:
#' \describe{
#'   \item{SubjectID}{Subject identifier}
#'   \item{VisitType}{Visit type (e.g., "D0" for day 0)}
#'   \item{stim}{Stimulation condition (e.g., "ebv")}
#'   \item{cyt_combn}{Cytokine combination pattern (e.g., "IFNg-IL2-TNF-IL17+")}
#'   \item{count_pop_den}{Count of cells in the denominator population}
#'   \item{count_pop_num}{Count of cells in the numerator population}
#' }
#' @source Internal dataset for package examples and testing
"data_count"
