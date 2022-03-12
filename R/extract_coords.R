#' @title Coordinates extraction
#' @description Extracts the bounding (edges) coordinates
#' of a polygon object.
#' @param poly (An sf or S4 object)
#' A polygon shapefile.
#' @usage extract_coords(poly)
#' @examples
#' data(camden_boundary)
#' extract_coords(poly=camden_boundary)
#' @details Given a spatial polygon object,
#' the function extracts its bounding coordinates.
#' @return Returns 2-column xy coordinates
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf st_coordinates
#' @importFrom dplyr select
#' @export

extract_coords <- function(poly){

  X <- Y <- as <- asNULL <- NULL

    #check the geometry of the input
    if(!class(poly)[1] %in% c("SpatialPolygonsDataFrame",
                             "SpatialPolygons", "sf")){
      stop(paste("Not the required object class!"))
    }

  #convert to as_spatial and retain the crs
  if(class(poly)[1] == "sf"){
    poly <- as(poly, 'Spatial') #convert#poly<- nc
  }

  #convert object class to simple feature (sf)
  poly2 <- st_as_sf(poly)
  poly2 <- data.frame(st_coordinates(poly2))
  poly2 <- poly2 %>%
    select(X, Y)

  return(poly2)
}


