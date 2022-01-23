#' @title Learning the spatiao-temporal properties of sample data
#' @description Learns all the spatial and temporal
#' properties (required to build events' population)
#' from a sample points.
#' @param ppt (matrix) coordinates (and time) vectors of points.
#' A 3-column matrix or list: `x` - eastings,
#' `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd'). The sample needs to
#' cover the one year period being investigated.
#' @param start_date (string) Specify the start date of
#' the sample point supplied (i.e. `ppt`),
#' in the format `yyyy-mm-dd`. If `NULL`, the earliest date
#' from the `ppt` is utilized. The end date is the 365th day
#' from the specified start date (by default).
#' @param poly (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). The boundary (a spatial polygon) surrounding
#' the sample points. Both `stps` and `poly` must be
#' in the same coordinate reference system. The default is `NULL`,
#' wherein an arbitrary boundary is created around the sample
#' points.
#' @usage stp_learner(ppt, start_date = NULL, poly = NULL)
#' @examples
#' data(SanF_fulldata)
#' #get a sample data
#' set.seed(1000)
#' sample_size <- 500
#' dat_sample <- SanF_fulldata[sample(1:nrow(SanF_fulldata),
#' sample_size, replace=FALSE),]
#' stp_learner(dat_sample,
#' start_date = NULL, poly = NULL)
#' @details Returns the spatiotemporal details of point datasets
#' @references https://www.google.co.uk/
#' @importFrom dplyr select
#' @export
stp_learner <- function(ppt=dat_sample, start_date = NULL, poly = NULL){

  #check if boundary is supplied,
  #if null, create an arbitrary boundary
  if(is.null(poly)){
    ppt_xy <-
      matrix(as.numeric(ppt[,1:2]),,2)
    boundary_ppt <- chull_poly(ppt_xy)
  } else {
    boundary_ppt <- poly
  }

  #check if start_date is supplied, if not
  #extract from the point data.
  if(is.null(start_date)){

  }
  # else{
  #   if(date_checker(start_date)==FALSE)
  # }
  #
  # date_checker(as.Date("2000-01-01"))

  #learn spatial properties
  #1.
  #2. derive spatial origins
  #impose square grids
  #is boundary supplied yes
  #if no create chull poly..

}
