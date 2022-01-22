#' @title Generate boundary around a Set of Points
#' @description Generate a boundary (polygon) around
#' a set of sample points, using Convex Hull technique.
#' @param xycoords (dataframe) coordinate vectors of points.
#' A 2-column matrix or list: `x` - the eastings,
#' and `y` - the northing.
#' @param crsys (string) coordinate reference system of
#' the resulting polygon. e.g.,
#' "+proj=longlat +datum=WGS84".
#' Default: \code{NULL}.
#' @usage chull_poly (xycoords,
#' crsys = NULL)
#' @examples
#' @details The algorithm is that given by Eddy (1977).
#' @references Eddy, W. F. (1977).
#' A new convex hull algorithm for planar sets.
#' ACM Transactions on Mathematical Software,
#' 3, 398--403.10.1145/355759.355766.
#' @importFrom grDevices chull
#' @importFrom sp SpatialPolygons Polygons
#' proj4string SpatialPolygonsDataFrame CRS
#' @export
chull_poly <- function(xycoords, crsys = NULL){

  set.seed(1)
  dat <- xycoords
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

  # plot(dat, pch=19)
  # lines(coords, col="red")

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

  return(sp_poly_df)
}

#crsys = "+proj=longlat +datum=WGS84"
#CRS(crsys)
