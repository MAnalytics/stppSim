#' @title Test the geometry and CRS of a polygon
#' @description To test that a polygon has the correct
#' geometry as well as a linear or Cartesian CRS.
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon representing a
#' landscape coverage.
#' @usage poly_tester(poly)
#' @examples
#' @details
#' @return Returns error messages if
#' the polygon is not in correct geometry
#' or CRS.
#' @references
#' #https://google.co.uk
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @export

poly_tester <- function(poly){

  as <- NULL

#check the polygon type
#and create the boundary
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
      #need to convert to spatvector class first.
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

