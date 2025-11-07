# Copilot Instructions for UtilsCytoRSV

This is an R package repository for utility functions for working with cytometry data (CyTOF and flow cytometry). The package provides data processing, calculation, and visualization tools for cytometry analysis.

## Code Standards

### Required Before Each Commit
- Run `devtools::document()` to update documentation when functions or parameters are modified
- Run `devtools::check()` to ensure the package passes R CMD check
- Ensure all new functions are properly documented with roxygen2 comments
- Update `README.Rmd` (not `README.md` directly) when adding significant new functionality, then knit to generate `README.md`

### Development Flow
- Build: `devtools::load_all()` or `devtools::build()`
- Test: `devtools::test()` or `testthat::test_dir("tests/testthat")`
- Check: `devtools::check()` to run R CMD check
- Document: `devtools::document()` to generate documentation from roxygen2 comments
- Install dependencies: The package uses `renv` for dependency management. Dependencies are listed in the `DESCRIPTION` file.

## Repository Structure
- `R/`: Core R functions for cytometry data analysis
  - `plot_cyto.R`, `plot_cyto_grid.R`: Visualization functions for cytometry data
  - `subtract_background.R`: Background subtraction utilities
  - `calc_proportions.R`, `sum_over_markers.R`: Data calculation functions
  - `chnl_lab.R`: Channel labeling utilities
  - Other utility functions for data processing
- `tests/testthat/`: Unit tests using the testthat framework
- `man/`: Auto-generated documentation (do not edit directly)
- `data/`: R data objects included with the package
- `DESCRIPTION`: Package metadata and dependencies
- `NAMESPACE`: Auto-generated from roxygen2 (do not edit directly)
- `.github/workflows/copilot-setup-steps.yml`: Setup steps for Copilot's development environment

## Key Guidelines

1. **Follow R package development best practices**
   - Use roxygen2 for all function documentation with `@title`, `@description`, `@param`, `@return`, `@examples`, and `@export` tags
   - Follow existing code style in the package (tidyverse style preferred)
   - Use the pipe operator `|>` (native R pipe) consistently as shown in existing code

2. **Maintain existing package structure**
   - All functions go in the `R/` directory with one function per file typically
   - Each exported function should have corresponding tests in `tests/testthat/test-<function-name>.R`
   - Use `@export` roxygen tag for functions that should be available to package users

3. **Dependencies**
   - This package uses `renv` for dependency management
   - Dependencies are specified in the `DESCRIPTION` file under `Imports` (required) or `Suggests` (optional)
   - Core dependencies include: `tibble`, `purrr`, `ggplot2`, `cowplot`, `dplyr`, `stringr`
   - Suggested packages include Bioconductor packages like `flowCore` and `flowWorkspace`

4. **Testing**
   - Write unit tests for all new functionality using `testthat` (edition 3)
   - Follow existing test patterns in `tests/testthat/`
   - Tests should use `test_that()` blocks with descriptive names
   - For functions requiring Bioconductor packages, use `.install_pkg_bioc()` helper (defined in `R/install.R`) to conditionally install packages like `flowCore` and `flowWorkspace` as shown in existing tests

5. **Documentation**
   - All exported functions must have complete roxygen2 documentation
   - Include working examples in `@examples` sections
   - Update `README.Rmd` when adding significant user-facing features
   - Documentation is generated with `devtools::document()` which updates `man/` and `NAMESPACE`

6. **Cytometry-specific considerations**
   - This package works with CyTOF and flow cytometry data
   - Functions often work with tibbles/data.frames where rows are cells and columns are markers/channels
   - Visualization functions use `ggplot2` with sensible defaults for cytometry data
   - Be aware of the distinction between markers (protein names) and channels (detector names)

7. **Code style**
   - Use tidyverse-style code as shown in existing functions
   - Prefer the native pipe `|>` over `%>%`
   - Use `dplyr` verbs for data manipulation
   - Function parameters should use snake_case with leading dots for data parameters (e.g., `.data`)
