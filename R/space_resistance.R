#' @include resistanceFromShape
#' @title Space (landscape) resistance raster
#' @description Build a space resistance map
#' from one or more shapefiles. Function builds
#' on functions from package raster- and
#' SimRIv-packages.
#' @param shp	either a character string specifying
#' the shapefile filename or a shapefile object itself.
#' @param baseRaster if provided, a raster onto which
#' to stack the given rasterized shapefile.
#' @param res	the desired pixel resolution of the
#' raster to be created, when baseRaster is not provided.
#' @param binary if TRUE, the shapefile will be rasterized
#' so that any feature is assigned a value of 0,
#' and the background 1.
#' @param field either a number in the range [0-1], in
#' which case it will be assigned to all pixels covered
#' by features of the shapefile; or the name of the numeric
#' shapefile field from which to extract such number; or
#' the name of the factor shapefile field containing classes
#' to map to resistance values (see mapvalues).
#' @param mapvalues a named vector specifying the resistance
#' value mapping from the classes of field.
#' @param extend set to TRUE to extend baseRaster if the shapefile
#' has a larger extent. If FALSE, the shapefile will be clipped
#' to baseRaster's extent.
#' @usage space_resistance(shp, baseRaster, res, binary = is.na(field),
#' field = NA, mapvalues = NA, extend = TRUE)
#' @examples
#' @details
#' @return Returns a trace of walker's path, and the
#' corresponding events.
#' @references
#' 1. Paul Murrell (2019). rasterize: Rasterize Graphical
#' Output. R package version 0.1.
#' https://CRAN.R-project.org/package=rasterize
#' 2. Quaglietta L, Porto M (2019). _SiMRiv: Individual-Based,
#' Spatially-Explicit Simulation and Analysis of Multi-State
#' Movements in River Networks and Heterogeneous Landscapes_.
#' R package version 1.0.4, <URL:
#' https://CRAN.R-project.org/package=SiMRiv>.
#' @export

space_resistance <- function(shp, baseRaster, res, binary = is.na(field),
                             field = NA,
                             mapvalues = NA, extend = TRUE){


  space_res <- resistanceFromShape(shp, baseRaster, res, binary = is.na(field)
                      , field = NA, background = 1, buffer = NA, margin = 0
                      , mapvalues = NA, extend = TRUE, ...)

  return(output)


}
