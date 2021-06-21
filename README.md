
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cytoutils

<!-- badges: start -->
<!-- badges: end -->

The goal of `cytoutils` is to facilitate common data processing and
visualisation tasks regarding cytometry data (with CyTOF and flow in
mind).

## Installation

You can install `cytoutils` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SATVILab/cytoutils")
```

## Examples

It provides a 2D hex plot with useful defaults.

``` r
library(cytoutils)
data('GvHD', package = 'flowCore')
ex_tbl <- flowCore::exprs(GvHD[[1]]) %>%
  tibble::as_tibble()
marker <- c("FL2-H", "FL3-H")
plot_cyto(
  data = ex_tbl, 
  marker = marker
)
```

<img src="man/figures/README-setup-1.png" width="100%" />

The ranges can be made equal between the x- and y-axes.

``` r
plot_cyto(
  data = ex_tbl, 
  marker = marker, 
  limits_equal = TRUE
)
```

<img src="man/figures/README-plot_cyto-limits_equal-1.png" width="100%" />

Each axis can be forced to include particular values (especially useful
if viewing gated data, which may have only positive-expressing cells and
you then want to show that there are no negative-expressing cells).

``` r
plot_cyto(
  data = ex_tbl, 
  marker = marker, 
  limits_expand = list(y = -5e3)
)
```

<img src="man/figures/README-plot_cyto-limits_expand-1.png" width="100%" />

You can get a vector to label channels as markers using `chnl_lab`, and
then supply this to `plot_cyto` to have better axis labels. Note that
the inverse function, `marker_lab`, is also available to convert from
markers to channels.

``` r
chnl_lab <- chnl_lab(GvHD)
plot_cyto(
  data = ex_tbl, 
  marker = marker, 
  lab = chnl_lab,
  limits_equal = TRUE
)
```

<img src="man/figures/README-plot_cyto-lab-1.png" width="100%" />
