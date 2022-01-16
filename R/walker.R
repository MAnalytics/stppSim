#' @title A landscape walker
#' @description A dynamic object capable of walking
#' across a constraint or unconstrained landscape,
#' in line with an in-built transition matrix, and
#' specified spatial and temporal properties. The transition
#' matrix defines a two-state status, namely `exploration`
#' and `action` states, to indicate the states of
#' reconnaissance and performance, respectively.
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
#' \code{Landscape Area/Number of origins * 100} for an unconstrained
#' landscape. Users are encouraged to input a value that
#' produce a desirable output.
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon defining the boundary
#' within which a walker walks. Default is \code{NULL}, where
#' a walker navigates an arbitrary square space with
#' coordinates c(0,0) at the bottom-left corner of the square
#' space.
#' @param coords a vector of the form c(x, y) giving the
#' initial coordinates of a walker (e.g. an origin).
#' Default: \code{c(0,0)}.
#' @param show.plot (TRUE or False) To show the time series
#' plot. Default is \code{FALSE}.
#' @usage walker(n = 5, s_threshold = 250, step_length = 20,
#' poly, coords=c(0,0), show.plot = FALSE)
#' @examples
#' @details
#' @return Returns a trace of walker's path, and the
#' corresponding events.
#' @references
#' #https://google.co.uk
#' @importFrom dplyr select filter
#' @importFrom SiMRiv species transitionMatrix
#' state.CRW simulate resistanceFromShape
#' @importFrom chron chron
#' @importFrom stats time
#' @export

walker <- function(n = 5, s_threshold = 250, step_length = 20,
                   poly, coords = c(0,0), show.plot = FALSE){

  points <- text <- sn <- x <- y <- X3 <- NULL

  #-----
  poly_tester(poly)
  #-----

  Walker <- species(
  state.CRW(0.005) + state.CRW(0.99),
  transitionMatrix(exp(-6.25679 + 1.26863*log(n)), #
                   0.70))
  #-6.25679 + 1.26863*log(n) is the
  #power regression that relate x and y (see 'calibra..R')

  #create the landscape resistance raster
  #create boundary
  landscape <- resistanceFromShape(poly, res = 20,
                                   buffer=15, background = 0.95, margin = 10)
  #plot(landscape)

  #does the point fall within the boundary?
  st_int <- st_intersects(st_as_sf(poly),
               st_point(coords, dim="XY"))
  st_int <- st_intersects(st_as_sf(poly),
                          st_point(c(526661.7, 185533.7), dim="XY"))

  st_int_yes <- as.numeric(st_int)

  plot(st_point(x = coords, dim = "XY"))

  #meaning 1-step/hrs
  Walker <- (Walker + step_length) * s_threshold
  sim <- simulate(Walker, time=200, resist = landscape, coords)#200 is the no of time.steps to be simulated
  #extract event locations
  sim_events <- data.frame(sim)
  colnames(sim_events) <- c("X1","X2","X3")
  sim_events <- sim_events %>%
    filter(X3 == 1)
  #200 (number of steps per origin)..
  #was used in the calibration ##nrow(sim) ##length(which(sim[,3]==1))

  #create hour sequence
  # hourly unit
  hm <- merge(0:23, seq(0, 0, by = 0))
  hour_seq <- chron(times. = paste(hm$x, ':', hm$y, ':', 0))

    #if event is present
    if(nrow(sim_events) >= 1){

      #assign random (but progressing) time to events
      hr_sample <- sample(hour_seq, nrow(sim_events), replace = TRUE)
      hr_sample <- hr_sample[order(hr_sample)]

      sim_events_ <- cbind(sim_events, 1:nrow(sim_events), hr_sample)

      if(show.plot == TRUE){
        plot(sim, type="l", asp=1, col="gray80")
        points(sim_events_, col="red")
        text(sim_events_[,1], sim_events_[,2],
         labels=sim_events_[,4], cex= 0.7, pos=3)
      }

      colnames(sim_events_) <- c("x","y","yes","sn", "time")

      sim_events_ <- data.frame(sim_events_) %>%
        dplyr::select(sn, x, y, time)

    return(sim_events_)
    }

}
