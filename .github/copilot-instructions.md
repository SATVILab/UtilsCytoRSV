# Copilot Instructions for UtilsCytoRSV

R package for cytometry data utilities (CyTOF and flow cytometry). Provides visualization, data processing, and calculation tools.

---

## Code Quality

- Make minimal, surgical changes to fix issues
- Maintain backward compatibility when possible
- Follow existing patterns in the codebase
- Add tests for new functionality or bug fixes
- Never leave trailing whitespace at the end of lines or on blank lines

---

## Before Committing

- Run `devtools::document()` to update documentation
- Run `devtools::test()` for faster iteration
- Run `devtools::check()` to ensure package passes R CMD check

---

## Package Structure

- `R/` - Source code (use `.` prefix for internal functions)
- `tests/testthat/` - Tests using testthat (edition 3)
- `man/` - Auto-generated docs (DO NOT edit directly)
- `data/` - R data objects included with package
- `DESCRIPTION` - Package metadata and dependencies
- `NAMESPACE` - Auto-generated from roxygen2 (DO NOT edit directly)

---

## R Coding Standards

### Function Naming

- Internal functions: prefix with `.` (e.g., `.plot_cyto_check`)
- Exported functions: descriptive snake_case (e.g., `plot_cyto`, `subtract_background`)

### Code Style

- Use tidyverse style
- Prefer native pipe `|>` over `%>%`
- Use `dplyr` verbs for data manipulation
- Function parameters: snake_case with leading dots for data (e.g., `.data`)

### Example - Correct

```r
#' @title Calculate proportions
#' @description Calculate proportions from numerator and denominator columns.
#' @param .data Data frame with counts.
#' @param den Character. Denominator column name.
#' @param num Character. Numerator column name.
#' @return A data frame with new proportion column.
#' @export
calc_prop <- function(.data, den, num) {
  .data |>
    dplyr::mutate(prop = .data[[num]] / .data[[den]])
}
```

### Example - Incorrect

```r
# Missing documentation, wrong pipe, no export tag
calcProp <- function(data, den, num) {
  data %>% mutate(prop = data[[num]] / data[[den]])
}
```

---

## Documentation

- All exported functions must have complete roxygen2 documentation
- Required tags: `@title`, `@description`, `@param`, `@return`, `@export`
- Include working `@examples` sections
- Update `README.Rmd` (not `README.md` directly) for new features

---

## Testing

- Write unit tests for all new functionality
- Use `test_that()` blocks with descriptive names
- Follow existing patterns in `tests/testthat/`
- For Bioconductor packages, use `.install_pkg_bioc()` helper

### Example Test

```r
test_that("calc_prop calculates proportions correctly", {
  mock_data <- tibble::tibble(count = 100, total = 1000)
  result <- calc_prop(mock_data, den = "total", num = "count")
  expect_equal(result$prop, 0.1)
})
```

---

## Dependencies

- Managed via `renv` and `DESCRIPTION` file
- Core: `tibble`, `purrr`, `ggplot2`, `cowplot`, `dplyr`, `stringr`
- Suggested: Bioconductor packages (`flowCore`, `flowWorkspace`)
- Add to `Imports` (required) or `Suggests` (optional) in DESCRIPTION

---

## Cytometry-Specific Notes

- Data structure: tibbles/data.frames with rows = cells, columns = markers/channels
- Markers = protein names, Channels = detector names
- Visualization uses `ggplot2` with sensible defaults

---

## Maintaining These Instructions

When updating copilot instructions, follow these best practices:

- Keep it concise - Files under 1000 lines (ideally under 250)
- Structure matters - Use headings, bullets, clear sections
- Be direct - Short, imperative rules over long paragraphs
- Show examples - Include code samples (correct and incorrect patterns)
- No external links - Copilot won't follow them; copy info instead
- No vague language - Avoid "be more accurate", "identify all issues", etc.
- Path-specific - Use `applyTo` frontmatter in topic files
