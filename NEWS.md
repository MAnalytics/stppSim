# stppSim 1.3.2

##Bug Fixing

-Correcting a bug relating to event point collation


## Bug Fixing:

- Resolved the issue of dependencies on retiring package (`rgdal`): 'https://github.com/MAnalytics/stppSim/issues/2'

## New changes:

- Introduced two new parameters, `s_band` and `t_band`, to the `psim_artif` function.
- Developed a new function named `snap_points_to_line`, which has been incorporated into both psim_artif and psim_real functions.


# stppSim 1.3.0

## Bug Fixing:

- Resolved the issue of dependencies on retiring package (`rgdal`): 'https://github.com/MAnalytics/stppSim/issues/2'

## New changes:

- Introduced two new parameters, `s_band` and `t_band`, to the `psim_artif` function.
- Developed a new function named `snap_points_to_line`, which has been incorporated into both psim_artif and psim_real functions.
- Added a new function named `NRepeat` for assessing space-time point interaction
- Incorporated a new argument, s_range, to `psim_real` function to establish the maximum spatial range.
- Introduced the `s_interaction` argument to `psim_real` function to determine the spatial properties of point interactions.
- Added the `tolerance` parameter, allowing users to set a p-value threshold for the detection of point interaction.


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
