#' @title Make square grids
#' @description Generates a system of square grids
#' over an area (boundary shapefile).
#' @param poly (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). A polygon object over
#' which square grids are to be created.
#' @param size Size of square grids to be
#' created. For example, the input `size`
#' for a 250 by 250 square grids is \code{250}.
#' @param show_output (logical) Display the output.
#' Default: \code{FALSE}
#' @param interactive (logical) to show
#' interactive map of the grids generated.
#' Default: \code{FALSE}.
#' @usage make_grids(poly, size = 250,
#' show_output = FALSE, interactive = FALSE)
#' @examples
#' #load boundary of Camden
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' boundary = camden$boundary
#' make_grids(poly=boundary, size = 250,
#' show_output = FALSE, interactive = FALSE)
#' @details Generates a square grid system in a shapefile
#' format (in the same `crs` as the input `poly`).
#' If `interactive` argument is `TRUE`, an interactive
#' map is shown from which the centroid coordinates
#' of any grid can be displayed by hovering the mouse
#' over the grid. If internet connection is
#' available on the PC, a basemap (OpenStreetmap) is
#' added to help identify places.
#' @return Returns a "SpatialPolygonsDataFrames" object
#' representing a system of square grids covering
#' the polygon area.
#' @importFrom sp spTransform proj4string bbox
#' SpatialPolygons SpatialPolygonsDataFrame CRS
#' Polygon Polygons
#' @importFrom sf st_as_sf as_Spatial
#' st_intersection
#' @importFrom dplyr select
#' @importFrom rgdal writeOGR
#' @importFrom terra linearUnits rast
#' @importFrom raster raster extent<- res<- crs<-
#' @importFrom leaflet leaflet addLayersControl
#' addTiles addPolygons labelOptions
#' @export
make_grids <- function(poly, size = 250, show_output = FALSE,
                       interactive = FALSE){

  show.output <- intersect_grid <- as <-
    geometry <- grid_ID <- st_transform <-
    addLayersControl <- layersControlOptions <-
    addPolygons <- labelOptions <- NULL

  extent <- crs <- res <- NULL

  area_B <- poly

  #-----
  poly_tester(poly)
  #-----

  #get coordinates
  proj_Coods <- suppressWarnings(proj4string(area_B))

  #3.Size of grid unit to create (in metres)
  #e.g. 50m, 100m, 150m, and so on.
  g_size = size

  #extracting the bounding coordinates of the boundary
  b_coord <- bbox(area_B)

  # Offsetting to create space for enough grids
  min_x <- round(b_coord[1,1], digits=-2)- (2*size) #
  min_y <- round(b_coord[2,1], digits=-2)- (2*size)
  max_x <- round(b_coord[1,2], digits=-2)+ (2*size)
  max_y <- round(b_coord[2,2], digits=-2)+ (2*size)

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
 square=cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus,
              xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)

 # Create spatial grid unit system (polygons)---
 #this requires WGS84 CRS as input below
 polys <- SpatialPolygons(mapply(function(poly, id) {
   xy <- matrix(poly, ncol=2, byrow=TRUE)
   Polygons(list(Polygon(xy)), ID=id)
 }, split(square, row(square)), ID), proj4string=CRS(proj_Coods) )


 # Create SpatialPolygonDataFrame --
 #this step is required to output multiple polygons.
 polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

 # Clipping the intersecting grid units with the boundary
 # List of grids that intersect the boundary
 area_B <- spTransform(area_B, CRS(proj_Coods))

 int_idx <- data.frame(st_intersects(st_as_sf(polys.df),
                 st_as_sf(area_B)))

 int_idx <- int_idx[,1]

 intersect_grids <- st_as_sf(polys.df[int_idx, ])

 intersect_grids <- intersect_grids %>%
   dplyr::select(id)

 #Visulising the results
 if(show_output == TRUE & interactive == FALSE){
   plot(intersect_grids, col="white")
 }

 #Visulising the results
 if((show_output == TRUE & interactive == TRUE) |
    (show_output == FALSE & interactive == TRUE)){
   #add grid ids
   intersect_grids$grid_ID <- 1:nrow(intersect_grids)

   #create points
   poly_geom <- intersect_grids %>%
     st_centroid()

   poly_geom$x <- st_coordinates(poly_geom)[,1]
   poly_geom$y <- st_coordinates(poly_geom)[,2]

   xy <- poly_geom %>%
     data.frame()%>%
     dplyr::select(-c(geometry))%>%
     dplyr::select(grid_ID, x, y)

   #first project to wgs 84
   sqgrids <- st_transform(intersect_grids,
                           crs = 4326)
   #join
   sqgrids_ <- sqgrids %>%
     left_join(xy)


   map <- leaflet() %>%
     addTiles()%>% #use a more detailed basemap
     addPolygons(data = sqgrids_, weight = 1,
                 label = ~paste("x:", x, "y:", y, sep=" "),
                 labelOptions = labelOptions(
                   noHide = F, textOnly = F,
                   textsize = 55,
                   style = list('color' = "red")
                 ),
                 color = ~colorQuantile("black", sqgrids_$grid_ID)(grid_ID),
                 fillOpacity = 0,
                 group = "square grids") %>%
     addLayersControl(overlayGroups = c("square grids"),
                      options = layersControlOptions(collapsed = FALSE))

   flush.console()
   print(map)
  }

 #convert back to polygon dataframe
 #to export
 intersect_grids <- as_Spatial(intersect_grids)

 return(intersect_grids)

}
