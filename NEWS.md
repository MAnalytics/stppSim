# stppSim 1.2.7

## Bug Fixes

- `artif_spo()` now utilizes `raster::projection()` instead of `raster::crs()` assign a coordinate system to a simple point feature.
projection(): 


# stppSim 1.2.1

## Changes

- Included a `vignette` to guide the use of the package
- `compare_boundary()` added to compare areas of two boundaries
- `make_grids()` function now include `interactive` argument the control plotting  
- New dataset `birmingham_boundary.rda` included in `inst/extdata`
- `DESCRIPTION` modified (R (>= 4.0.0) to R (>= 4.1.0))

