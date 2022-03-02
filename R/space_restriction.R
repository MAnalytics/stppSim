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
#' so that any feature is assigned a value of 0 (least resistance
#' to event occurences),
#' and the background 1 (most resistance to event occurences).
#' @param field (string) the name of the character
#' field from which to extract the label of different
#' feature classes.
#' @param mapvalues a named vector specifying the resistance
#' value mapping from the classes of field.
#' @param background the value in the range 0 and 1
#' to assign to all pixels that are not covered
#' by any shapefile feature.
#' @usage space_restriction(shp, baseMap, res, binary = is.na(field),
#' field = NA, mapvalues = NA, background = 1)
#' @examples
#' data(camden_boundary)
#' restrct_space <- space_restriction(shp = camden_boundary,
#' res = 100, binary = TRUE)
#' #plot the result
#' #plot(restrct_space)
#' #Setting 'restrct_space' raster as basemap, the landuse
#' #map can now be stacked onto the basemap as follows:
#' data(camden_landuse)
#' restrct_Landuse <- space_restriction(shp = camden_landuse,
#' baseMap = restrct_space,
#' res = 100, field = "type", mapvalues = c("Green" = 0.8,
#' "Leisure" = 1, "Sports" = 0.7), background = 1)
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

space_restriction <- function(shp, baseMap, res=100, binary = is.na(field),
                             field = NA,
                             mapvalues = NA, background = 1){


  space_res <- resistanceFromShape(shp = shp, baseRaster = baseMap, res = res,
                                   binary = binary, field = field,
                                   background = background, buffer = NA,
                                   margin = 0,
                                   mapvalues = mapvalues, extend = FALSE)

  return(space_res)

}
