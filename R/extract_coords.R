#' @title Extracting coordinates of a polygon boundary
#' @description Given a polygon object, the goal is to extract
#' the coordinates of the edges of the boundary.
#' @param poly (a spatialPolygons, spatialPolygonDataFrames, or
#' an "sf").
#' The polygon object must be in a
#' Cartesian coordinate reference system.
#' @usage extract_coords(poly)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references https://www.google.co.uk/
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf st_coordinates
#' @importFrom dplyr select
#' @export

extract_coords <- function(poly){

  X <- Y <- NULL

    #check the geometry of the input
    if(!class(poly)[1] %in% c("SpatialPolygonsDataFrame",
                             "SpatialPolygons", "sf")){
      stop(paste("Not the required object class!"))
    }

  #convert to as_spatial and retain the crs
  if(class(poly)[1] == "sf"){
    poly <- as_Spatial(poly) #convert#poly<- nc
  }

  #convert object class to simple feature (sf)
  poly2 <- st_as_sf(poly)
  poly2 <- data.frame(st_coordinates(poly2))
  poly2 <- poly2 %>%
    select(X, Y)

  return(poly2)
}


