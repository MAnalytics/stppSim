#' @title Learning the spatiao-temporal properties of sample data
#' @description Learns all the spatial and temporal
#' properties (required to build events' population)
#' from a sample point dataset.
#' @param stps (dataframe)
#' Spatiotemporal point samples - whose first three attributes
#' (`fields`) are: `x` - the eastings, `y` - the northing,
#' and `t`, the time of occurence (in the format: `yyyy-mm-dd').
#' A minimum of (first) three-month worth of data is required. The
#' longer the data duration, the better the approximations.
#' @param `poly` (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). The boundary (a spatial polygon) surrounding
#' the extent of the point sample. Both `stps` and `poly` must be
#' in the same coordinate reference system. The default is `NULL`,
#' meaning that the function creates an arbitrary boundary around
#' the points.
#' @usage stp_learner(stp)
#' @examples
#' @details Returns the spatiotemporal details of point datasets
#' @references https://www.google.co.uk/
#' @importFrom dplyr select
#' @export
stp_learner <- function(stp){

  set.seed(1)
  dat <- matrix(stats::rnorm(2000), ncol = 2)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

  plot(dat, pch=19)
  lines(coords, col="red")

  set.seed(1)
  dat <- matrix(stats::rnorm(2000), ncol = 2)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

  plot(dat, pch=19)
  lines(coords, col="red")





}
