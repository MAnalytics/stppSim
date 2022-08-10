# stppSim 1.2.7

## Bug Fixing

- `artif_spo()` now utilizes `raster::projection()` instead of `raster::crs()` assign a coordinate system to a simple point feature.

# stppSim 1.2.1

## Changes

- Included a `vignette` to guide the use of the package
- `compare_boundary()` added to compare areas of two boundaries
- `make_grids()` function now include `interactive` argument the control plotting  
- New dataset `birmingham_boundary.rda` included in `inst/extdata`
- `DESCRIPTION` modified (R (>= 4.0.0) to R (>= 4.1.0))

# stppSim 1.2.0

## Changes

- Removed `set.seed` from `chull_poly()`
- Added `mfocal` argument to `artif_spo()` and `psim_artif()` functions.

# stppSim 1.1.0

## Changes

- The length of the title reduced to less than 65 characters.
- Added references to the `DESCRIPTION` file
- Added `\value` field to `.Rd` files regarding exported methods. Also, included explanations of the function results.
- Suppressed unnecessary messages to the console.
- Removed parallel processing functions altogether
- Removed seed set within functions

# stppSim 1.0.0

- First CRAN submission
