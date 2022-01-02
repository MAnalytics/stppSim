#' @title Modeling of the Global Temporal Pattern
#' @description Models the global temporal pattern (of
#' the point process) as consisting of the global linear
#' trend and the seasonality.
#' @param poly (a list or dataframe) A list of spatial boundary
#' coordinates (or shapefile) within which the events are confined.
#' The shapefile can be
#' @param start_date The start date of the study period.
#' Default value is `"01-01"` (i.e. January 1st). By default
#' the end date of the study period is set as `"12-31"` (i.e.
#' 31st December). A user can specify any start date in the
#' format `"mm/dd"`. The end date is the next 365th day
#' from the specified start date.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
#' @param t_threshold (character) The temporal threshold
#' at which events are re-generated (or repeated) by
#' the same origin.
#' Default:\code{"daily"}. Other values are:
#' \code{"weekly", "monthly"}.
#' @param trend (a character) Specifying the direction of
#' the global (linear) trend of the point process. Three options
#' available are `"decreasing"`, `"stable"`,
#' and `"increasing"` trends. Default: `"stable"`.
#' @param slope (a character) Slope angle for an
#' "increasing" or "decreasing" trend. Two options
#' are available: `"gentle"` and `"steep"`.
#' Default value is \code{"NULL"} for the default trend
#' (i.e. `stable`).
#' @param first_s_peak Number of days before the first seasonal
#' peak. Default: \code{90}. This implies a seasonal cycle
#' of 180 days.
#' @param scale (an integer) For scaling point counts. Default: \code{1}
#' @param npoints (an integer) Number of origins (points) to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' points (origins).
#' @param show.plot (TRUE or False) To show the time series
#' plot. Default is \code{FALSE}.
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @export
#'

psim <- function(start_date, trend, s_threshold, slope, first_s_peak, scale,
                 poly, npoints, p_ratio, show.plot){


  LevyWalker <- species(
    state.RW() + state.CRW(0.99),
    transitionMatrix(0.005, 0.02))

  levy.walker <- (levy.walker + step_length) * 10 #p_range

  sim <- simulate(LevyWalker, 200)
  plot(sim, type="l", asp=1)

  length(which(sim[,3]==1))


   #simulate movement #750 * 10 = 7.5km covered in two hours in two days.
  sim.lw.road <- simulate(levy.walker, 365,
                          resist = landuse_map, coords = init)

  #combine the date of occurence  mode(sim.lw.road)
  sim.lw.road <- cbind(sim.lw.road, as.data.frame(start_Date))
  colnames(sim.lw.road) <- c("x", "y", "state", "date")



}




