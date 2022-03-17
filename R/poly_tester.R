#' @title Geometry and Coordinate
#' Reference System test of a polygon
#' @description Tests whether a polygon
#' has the correct geometry,
#' namely; `S4` or `sf`. Also, tests
#' that there is a valid projection attached to
#' the polygon.
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon object.
#' @usage poly_tester(poly)
#' @examples
#' #load boundary of Camden
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' boundary = camden$boundary # get boundary
#' poly_tester(poly=boundary)
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

  # #check the polygon geometry
     if(isS4(poly)){
  #     #check the geometry of the input
  #     if(!class(poly)[1] %in% c("SpatialPolygonsDataFrame",
  #                               "SpatialPolygons", "sf")){
  #       stop(paste("Not the required object class!"))
  #     }

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
      crs(area_B.raster) <- suppressWarnings(proj4string(area_B))
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

