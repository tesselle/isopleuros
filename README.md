
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isopleuros

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/isopleuros/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/isopleuros/actions)
[![codecov](https://codecov.io/gh/tesselle/isopleuros/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tesselle/isopleuros)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/isopleuros/badge/main)](https://www.codefactor.io/repository/github/tesselle/isopleuros/overview/main)

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

Ternary plots made simple. **isopleuros** allows to create ternary plot
using **graphics**. It provides functions to display the data in the
ternary space, to add or tune graphical elements and to display
statistical summaries. It also includes common ternary diagrams useful
for the archaeologist (e.g. soil texture charts, ceramic phase diagram).

**isopleuros** is a dependency-free package designed to be as simple as
possible. If you need finer tuning or more advanced features, you should
consider the [**Ternary**](https://ms609.github.io/Ternary/) or
[**ggtern**](http://www.ggtern.com/) package.

## Installation

You can install the released version of **isopleuros** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("isopleuros")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/isopleuros")
```

## Usage

``` r
## Install extra packages (if needed)
# install.packages("folio")

## Load packages
library(folio) # Datasets
library(isopleuros)
```

**isopleuros** tries to mimic the way **graphics** works as much as
possible. This means that you should not be confused if you are familiar
with the standard R graphics environment.

``` r
## Data from Barrera and Velde 1989
data("verre", package = "folio")

## Select data
coda <- verre[, c("Na2O", "CaO", "K2O")]

## Ternary plot
ternary_plot(coda, panel.first = ternary_grid())

## Split data
groups <- split(coda, f = coda$Na2O > 5)

## Add tolerance ellipses
for (group in groups) {
  ternary_tolerance(group, level = 0.95, border = "blue", lty = 2)
}
```

<img src="man/figures/README-ternary-1.png" style="display: block; margin: auto;" />

``` r
## Select data
coda <- verre[, c("Na2O", "CaO", "K2O", "MgO", "P2O5", "SiO2", "Al2O3")]

## Ternary plots with marginal compositions
ternary_pairs(coda, col = as.factor(coda$Na2O > 5))
```

<img src="man/figures/README-pairs-1.png" style="display: block; margin: auto;" />

## Contributing

Please note that the **isopleuros** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
