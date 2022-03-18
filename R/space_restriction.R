#' @title Space restriction raster map
#' @description Builds a space restriction map
#' from one or more shapefiles. A space restriction
#' raster map showing the restriction levels
#' of various features across the landscape.
#' The function builds
#' on raster- and SimRIv-packages.
#' @param shp	shapefile object containing
#' features to serve as obstructions to the movement
#' of walkers.
#' @param baseMap if provided, a raster onto which
#' to stack the restriction features (`shp`).
#' @param res	the desired pixel resolution of the
#' raster to be created, when baseMap is not provided.
#' @param binary if TRUE, the shapefile will be rasterized
#' so that all features are assigned a value of `0` (minimum
#' restriction level),
#' and the background is assigned `1`
#' (maximum restriction level).
#' @param field a number in the range of \code{[0-1]}
#' (i.e. restriction values) assigned
#' to all features; or
#' the name of a numeric field to extract such
#' restriction values for different classes of
#' feature.
#' Restriction value `0` and `1` indicate the
#' lowest and the highest obstructions, respectively.
#' Default: \code{NULL}.
#' @param background the value in the range 0 and 1
#' to assign to all pixels that are not covered
#' by any shapefile object.
#' @usage space_restriction(shp, baseMap, res, binary = is.na(field),
#' field = NA, background = 1)
#' @examples
#' #load boundary of Camden and land use data
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' boundary = camden$boundary # get boundary
#' restrct_map <- space_restriction(shp = boundary,
#' res = 20, binary = TRUE)
#' #plot the result
#' #plot(restrct_space)
#' #Setting 'restrct_space' raster as basemap, the landuse
#' #map can now be stacked onto the basemap as follows:
#' landuse = camden$landuse # get landuse
#' restrct_Landuse <- space_restriction(shp = landuse,
#' baseMap = restrct_map,
#' res = 20, field = "rValues2", background = 1)
#' #plot(restrct_Landuse)
#' @details Helps to create a complete space restriction map
#' with cell values ranging from 0
#' (`minimum restriction level`) and 1(`maximum restriction
#' level`). All
#' other areas not covered by any features are assigned the value
#' of `background`. When stacking additional features to existing
#' `baseMap`, only the areas covered by features are updated, while
#' the remaining areas retain the original values of `baseMap`.
#' @return Returns a raster map showing the restriction
#' levels across the landscape.
#' @references
#' 1. Paul Murrell (2019). rasterize: Rasterize Graphical
#' Output. R package version 0.1.
#' https://CRAN.R-project.org/package=rasterize
#' 2. Quaglietta L, Porto M (2019). _SiMRiv: Individual-Based,
#' Spatially-Explicit Simulation and Analysis of Multi-State
#' Movements in River Networks and Heterogeneous Landscapes_.
#' R package version 1.0.4, <URL:
#' https://CRAN.R-project.org/package=SiMRiv>.
#' @importFrom SiMRiv resistanceFromShape
#' @export

space_restriction <- function(shp, baseMap, res=20, binary = is.na(field),
                             field = NA, background = 1){

  suppressWarnings(
  space_res <- resistanceFromShape(shp = shp, baseRaster = baseMap, res = res,
                                   binary = binary, field = field,
                                   background = background, buffer = NA,
                                   margin = 0,extend = FALSE))

  return(space_res)

}
