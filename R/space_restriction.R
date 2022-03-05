#' @title Space restriction map (raster)
#' @description Build a space restriction map
#' from one or more shapefiles. A space restriction map
#' (raster) shows the restrictions (to event occurences)
#' across space. Function builds
#' on raster- and SimRIv-packages.
#' @param shp	polygon shapefile object.
#' @param baseMap if provided, a raster onto which
#' to stack the next rasterized shapefile.
#' @param res	the desired pixel resolution of the
#' raster to be created, when baseMap is not provided.
#' @param binary if TRUE, the shapefile will be rasterized
#' so that any feature is assigned a value of `0`,
#' and the background is assigned `1`.
#' @param field A number in the range of \code{[0-1]}
#' (i.e. resistance values) to
#' assign to all features covered by `shp`; or
#' the name of a numeric field to extract such
#' resistance values for different feature classes.
#' The resistance value `0` and `1` indicate the
#' lowest and the highest restrictions, respectively,
#' to an event occuring within the space occupied
#' by a feature.
#' @param background the value in the range 0 and 1
#' to assign to all pixels that are not covered
#' by any shapefile feature.
#' @usage space_restriction(shp, baseMap, res, binary = is.na(field),
#' field = NA, background = 1)
#' @examples
#' data(camden_boundary)
#' restrct_space <- space_restriction(shp = camden_boundary,
#' res = 20, binary = TRUE)
#' #plot the result
#' #plot(restrct_space)
#' #Setting 'restrct_space' raster as basemap, the landuse
#' #map can now be stacked onto the basemap as follows:
#' data(cam_landuse)
#' restrct_Landuse <- space_restriction(shp = landuse,
#' baseMap = restrct_space,
#' res = 20, field = "rValues2", background = 1)
#' #plot(restrct_Landuse)
#' @details Help to create a complete space restriction map
#' with cell values ranging from 0(lowest) and 1(highest). All
#' other area not covered by any features is assigned the value
#' of `background`. When stacking additional features to existing
#' `baseMap`, only the areas covered by features are updated, while
#' the remaining areas retain the original values of `baseMap`.
#' @return Returns a raster map
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


  space_res <- resistanceFromShape(shp = shp, baseRaster = baseMap, res = res,
                                   binary = binary, field = field,
                                   background = background, buffer = NA,
                                   margin = 0,extend = FALSE)

  return(space_res)

}
