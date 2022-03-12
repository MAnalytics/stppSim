#' @title Boundary surrounding a set of points
#' @description Generates a boundary (polygon) around
#' a set of points, using Convex Hull technique
#' (Eddy, W. F, 1977).
#' @param xycoords (matrix) A 2-column
#' coordinate vectors of points: `x` - the eastings,
#' and `y` - the northing.
#' @param crsys Optional string specifying the coordinate
#' reference system (crs) of the resulting boundary, e.g.,
#' the crs string "+proj=longlat +datum=WGS84" transform
#' the resulting boundary to wgs84 system.
#' @usage chull_poly(xycoords,
#' crsys = NULL)
#' @examples
#' data(xyt_data)
#' #extract xy coordinates only
#' xy <- matrix(as.numeric(xyt_data[,1:2]),,2)
#' bry <- chull_poly(xy, crsys = NULL)
#' #visualise result
#' #plot(bry) #to plot
#' #points(xy[,1], xy[,2], add=TRUE)
#' @details Draws an arbitrary boundary around
#' spatial points by joining the outer-most
#' points by lines.
#' @references Eddy, W. F. (1977).
#' A new convex hull algorithm for planar sets.
#' ACM Transactions on Mathematical Software,
#' 3, 398--403.10.1145/355759.355766.
#' @importFrom grDevices chull
#' @importFrom sp SpatialPolygons Polygons
#' proj4string<- SpatialPolygonsDataFrame CRS
#' @export
chull_poly <- function(xycoords, crsys = NULL){

  proj4string <- NULL

  set.seed(1)
  dat <- xycoords
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

   if(is.null(crsys)){
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  }

  if(!is.null(crsys)){
    sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)),
                                             ID=1)))
    proj4string(sp_poly) <- crsys
    # set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
    # e.g. CRS("+proj=longlat +datum=WGS84")
  }

  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))

  #plot(sp_poly_df)
  #spoint<-SpatialPoints(xycoords)
  #plot(spoint, add=TRUE)
  #writeOGR(sp_poly_df, "chull", layer="chull", driver="ESRI Shapefile")
  #flush.console()
  #plot(sp_poly_df)

  return(sp_poly_df)
}

#crsys = "+proj=longlat +datum=WGS84"
#CRS(crsys)
