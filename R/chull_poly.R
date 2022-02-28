#' @title Generate boundary around a set of Points
#' @description Generate a boundary (polygon) around
#' a set of sample points, using Convex Hull technique.
#' @param xycoords (matrix) coordinate vectors of points.
#' A 2-column matrix or list: `x` - the eastings,
#' and `y` - the northing.
#' @param crsys (string) coordinate reference system of
#' the resulting polygon. e.g.,
#' "+proj=longlat +datum=WGS84".
#' Default: \code{NULL}.
#' @usage chull_poly (xycoords,
#' crsys = NULL)
#' @examples
#' data(SanF_fulldata)
#' SanF_fulldata_xy <-
#' matrix(as.numeric(SanF_fulldata[,1:2]),,2)
#' bry <- chull_poly(SanF_fulldata_xy, crsys = NULL)
#' @details The algorithm is that given by Eddy (1977).
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
  #flush.console()
  #plot(sp_poly_df)

  return(sp_poly_df)
}

#crsys = "+proj=longlat +datum=WGS84"
#CRS(crsys)
