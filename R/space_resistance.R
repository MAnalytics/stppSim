#' @title Base (restriction) map (raster)
#' @description Build a base (restriction) map
#' from one or more shapefiles. A base (restriction) map
#' (raster) shows restriction features across
#' space. Function builds
#' on raster- and SimRIv-packages.
#' @param shp	polygon shapefile object.
#' @param baseRaster if provided, a raster onto which
#' to stack the given rasterized shapefile.
#' @param res	the desired pixel resolution of the
#' raster to be created, when baseRaster is not provided.
#' @param binary if TRUE, the shapefile will be rasterized
#' so that any feature is assigned a value of 0,
#' and the background 1.
#' @param field either a value in the range 0 to 1sa, in
#' which case it will be assigned to all pixels covered
#' by features of the shapefile; or the name of the numeric
#' field from which to extract such value; or
#' the name of the factor shapefile field containing classes
#' to map to resistance values.
#' @param mapvalues a named vector specifying the resistance
#' value mapping from the classes of field.
#' @usage basemap(shp, baseRaster, res, binary = is.na(field),
#' field = NA, mapvalues = NA)
#' @examples
#' @details
#' @return Returns a raster showing space resistance levels
#' or intensity to event occurrences.
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

basemap <- function(shp, baseRaster, res=100, binary = is.na(field),
                             field = NA,
                             mapvalues = NA){


  space_res <- resistanceFromShape(shp = shp, baseRaster = baseRaster, res = res,
                                   binary = is.na(field), field = NA,
                                   background = 1, buffer = NA, margin = 0,
                                   mapvalues = NA, extend = FALSE)

  return(space_res)

}
