#' @title Square Grids System
#' @description Generates a system of square grids over a specified
#' spatial boundary.
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon over
#' which the spatial grid is to be overlaid. Needs to be in a
#' cartesian CRS.
#' @param gridsize Square grid size to be generated.
#' To be in the same unit associated with the `poly` (e.g.
#' metres, feets, etc.). Default: \code{200}.
#' @param dir (character) Specifies the directory to
#' export the output. Default is `NULL`, indicating the
#' current working directory (cwd). A user can specify a different
#' directory in the format: "C:/.../folder".
#' @param show.output (logical) To show the output.
#' Default: \code{FALSE}
#' @usage sq_grids(poly, gridsize = 200,
#' show.output = FALSE, dir=NULL)
#' @examples
#' @details
#' @return Returns a spatial square grid system
#' in a shapefile format
#' @references https://www.google.co.uk/
#' @importFrom sp spTransform proj4string bbox
#' SpatialPolygons SpatialPolygonsDataFrame CRS
#' Polygon Polygons over
#' @importFrom rgdal writeOGR
#' @export

sq_grids <- function(poly, gridsize = 200, show.output = FALSE,
                       dir=NULL){

  as_Spatial <- show.plot <- intersect_grid <- NULL
  # check the crs
  # check area..and minimum are specified..

  #check the geometry of the input
  if(!class(poly)[1] %in% c("SpatialPolygonsDataFrame",
                            "SpatialPolygons", "sf")){
    stop(paste("Not the required object class!"))
  }

  #if simple feature is supplied
  #convert to as_spatial and retain the crs
  if(class(poly)[1] == "sf"){
    poly <- as_Spatial(poly) #convert#poly<- nc
  }

  area_B <- poly

  #check if polygon has the required 'crs'

  #get coordinates
  proj_Coods <- proj4string(area_B)

  #3.Size of grid unit to create (in metres) e.g. 50m, 100m, 150m, and so on.
  g_size = gridsize

  #extracting the bounding coordinates of the boundary
  b_coord <- bbox(area_B)

  # Offsetting to create space for enough grids
  min_x <- round(b_coord[1,1], digits=-2)- (2*gridsize) #
  min_y <- round(b_coord[2,1], digits=-2)- (2*gridsize)
  max_x <- round(b_coord[1,2], digits=-2)+ (2*gridsize)
  max_y <- round(b_coord[2,2], digits=-2)+ (2*gridsize)

  #Creating sequence of grid coordinates...
  x <- seq(min_x,max_x,by=g_size)
  y <- seq(min_y,max_y,by=g_size)

  #Generate centroid coordinates of each grid
  id <- 0

  centroid_points <- NULL

  for(i in 1: length(x)){
    for(j in 1:length(y)){
      id <- id + 1
      centroid_points <- rbind(centroid_points, cbind(id, x[i], y[j]))
    }
  }

  colnames(centroid_points) <- c("id", "x", "y")
  centroid_points <- as.data.frame(centroid_points)

  # Creating the coordinates of the four edges of each grid unit
  radius <- g_size/2 #radius in meters
  yPlus <- centroid_points$y+radius
  xPlus <- centroid_points$x+radius
  yMinus <- centroid_points$y-radius
  xMinus <- centroid_points$x-radius

 ID=centroid_points$id

 # Combining the edge coordinates for each unit
 square=cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)

 # Create spatial grid unit system (polygons)---this requires WGS84 CRS as input below
 polys <- SpatialPolygons(mapply(function(poly, id) {
   xy <- matrix(poly, ncol=2, byrow=TRUE)
   Polygons(list(Polygon(xy)), ID=id)
 }, split(square, row(square)), ID), proj4string=CRS(proj_Coods) )


 # Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
 polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

 # Clipping the intersecting grid units with the boundary
 # List of grids that intersect the boundary
 area_B <- spTransform(area_B, CRS(proj_Coods))
 intersect_grids <- polys.df %over% area_B
 intersect_grids <- polys.df[which(!is.na(intersect_grids[,1])),]

 #Visulising the results
 if(show.plot == TRUE){
    plot(intersect_grid)
 }

 #get output directory
 if(dir == "NULL"){
   dr <- getwd()
 }

 if(dir != "NULL"){
   dr <-dir
 }

 tryCatch(

   # Specifying expression
   expr = {
     #Exporting the grids created
     writeOGR(intersect_grids, dr, 'spatial_grid_system', 'ESRI Shapefile', overwrite_layer=T)
     print("Execution completed!")
   },
   # Specifying error message
   error = function(e){
     print("Execution halted! Check the specified output path")
   },

   finally = {
     print("finally Executed")
   }
 )


}
