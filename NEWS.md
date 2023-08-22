# stppSim 1.3.0

## Bug Fixing:

- Resolved the issue of dependencies on retiring package, rgdal: 'https://github.com/MAnalytics/stppSim/issues/2'

## New changes:
- Added two parameters `s_band` and  `t_band` to `psim_artif` function
- Created new function `snap_points_to_line` and integrated the function into `psim_artif` and `psim_real` functions.
- Added two new arguments `s_range` to set maximum spatial range, and `s_interaction` to specify type of point interation in simulation
- Added `tolerance` argument to set pvalue threshold for point interactions 
8. s_range. and s_interaction


# stppSim 1.2.7

## Bug Fixing

- `artif_spo()` now utilizes `raster::projection()` instead of `raster::crs()` to assign a coordinate system to a simple point feature.

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
