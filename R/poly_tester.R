#' @title Geometry and crs of a polygon
#' @description Test whether an input polygon
#' has the correct geometry,
#' namely `S4` or `sf`. Further, to test
#' that there is a valid projection attached to
#' the polygon.
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon
#' @usage poly_tester(poly)
#' @examples
#' poly_tester(poly=camden_boundary)
#' @details
#' Returns an error message if
#' the polygon is not in correct geometry
#' or CRS.
#' @return Returns error messages, or mute
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res
#' linearUnits
#' @export

poly_tester <- function(poly){

  as <- NULL

  #check the polygon geometry
    if(isS4(poly)){
      #check the geometry of the input
      if(!class(poly)[1] %in% c("SpatialPolygonsDataFrame",
                                "SpatialPolygons", "sf")){
        stop(paste("Not the required object class!"))
      }

      #if simple feature is supplied
      #convert to as_spatial and retain the crs
      if(class(poly)[1] == "sf"){
        poly <- as(poly, 'Spatial') #convert#poly<- nc
      }

      #To check if poly is in linear crs
      area_B <- poly
      #retrieve crs of polygon and test if
      #crs is cartesian
      area_B.raster <- raster()
      #Use extent (from raster package) to read
      #bounds of vector and assign to the raster:
      extent(area_B.raster) <- extent(area_B)
      crs(area_B.raster) <- proj4string(area_B)
      res(area_B.raster) <- 50 #assumed
      #now rasterize
      area_B.raster.r <- rast(area_B.raster)
      #now convert spatvector
      crstype <- linearUnits(area_B.raster.r) #returns zero if in wgs84

      if(crstype == 0){
        stop("Boundary's crs NOT in linear unit! Operation terminated!!")
      }
  }

}

