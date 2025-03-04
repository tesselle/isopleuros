# isopleuros 1.4.0
## New classes and methods
* Add `tile_bin()`, `tile_density()` and `tile_interpolate()` to compute heatmap values.

## Enhancements
* Translate into French.

# isopleuros 1.3.0
## New classes and methods
* Add `ternary_image()` to color a ternary plot according to the output of a function.

## Enhancements
* Allow to center and scale label coordinates.
* Generate diagnostic messages if coordinates are not centered/scaled when they should be.

# isopleuros 1.2.0
## Enhancements
* Allow to center and scale data before plotting.

# isopleuros 1.1.0
## New classes and methods
* Add `ternary_labels()` to draw non-overlapping text labels.

## Bugfixes & changes
* Prevent division by zero when calculating ternary coordinates.

## Internals
* Use **interp** instead of **akima**.
* Use **tinytest** and **tinysnapshot** instead of **testthat** and **vdiffr**.

# isopleuros 1.0.0

* First CRAN release.
