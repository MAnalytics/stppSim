#' @title A landscape walker
#' @description An object capable of walking
#' across a constraint or unconstrained landscape
#' landscape, in accordance with a specified
#' spatial and temporal properties. The object
#' may or may not generates event(s) while navigating
#' across the landscape, based on an embedded
#' transition matrix.
#' @param n (integer) The number of events
#' to be generated. Default: \code{5}.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
#' @param step_length (numeric) A maximum step taken at a time
#' by a walker from one state to the next. Should be a fraction
#' of the spatial units of the landscape. Default: half the size
#' of the minimum spatial unit in a landscape
#' (for a constraint landscape) or
#' \code(Landscape Area/Number of origins * 100) for an unconstrained
#' landscape. Users are encouraged to input a value that
#' produce a desirable output.
#'

#' @param show.plot (TRUE or False) To show the time series
#' plot. Default is \code{FALSE}.
#' @usage gtp(start_date = "01-01", trend = "stable",
#' slope = "NULL", first_s_peak=90, scale = 1, show.plot =FALSE)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @export
#'

walker <- function(n = 5, s_threshold = 250,
                   t_threshold = "daily",
                   show.plot = FALSE){

  Walker <- species(
  state.CRW(0.005) + state.CRW(0.99),
  transitionMatrix(exp(-6.25679 + 1.26863*log(n)), #
                   0.70))
  #-6.25679 + 1.26863*log(n) is the
  #power regression that relate x and y (see 'calibra..R')

  t_threshold <- 24 / 7


  # #check
  # if(!t_threshold %in% c("daily", "weekly", "monthly")){
  #   stop("'t_threshold' can only be 'daily', 'weekly', or 'monthly' ")
  # }
  #
  # #translate temporal threshold
  # if(t_threshold == "daily"){
  #   t_threshold <- 24
  # } else if(t_threshold == "weekly"){
  #   t_threshold <- 24 * 7
  # } else {
  #   t_threshold <- 24 * 30
  # }

  #meaning 1-step/hrs
  Walker <- (Walker + step_length) *s_threshold
  sim <- simulate(Walker, 200)
  #200 (number of steps per origin)..
  #was used in the calibration
  nrow(sim)
  length(which(sim[,3]==1))

  if(show.plot == TRUE){
    #extract event locations
    sim_events <- sim[which(sim[,3]==1),]
    sim_events_ <- cbind(sim_events, 1:nrow(sim_events))
    plot(sim, type="l", asp=1, col="gray80")
    points(sim_events_, col="red")
    text(sim_events_[,1], sim_events_[,2],
         labels=sim_events_[,4], cex= 0.7, pos=3)
    }


}
