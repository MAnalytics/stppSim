#' @include gtp.R
#' @include walker.R
#' @title Stpp from synthetic origins
#' @description Generates spatiotemporal
#' point patterns based on a set of
#' synthesized origins.
#' @param n_events number of points
#' (events) to simulate. Default: \code{1000}.
#' A vector of integer values can be supplied, such as,
#' c(`a`1, `a`2, ....)`, where `a`1, `a`2, ...
#' represent different integer values.
#' @param start_date the start date of the temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The GTP will normally cover a 1-year period.
#' @param poly (An sf or S4 object)
#' a polygon shapefile defining the extent of the landscape
#' @param n_origin number of locations to serve as
#' origins for walkers. Default:\code{50}.
#' @param restriction_feat (An S4 object) optional
#' shapefile containing features
#' in which walkers cannot walk through.
#' Default: \code{NULL}.
#' @param field a number in the range of \code{[0-1]}
#' (i.e. restriction values) assigned
#' to all features; or
#' the name of a numeric field to extract such
#' restriction values for different classes of
#' feature.
#' Restriction value `0` and `1` indicate the
#' lowest and the highest obstructions, respectively.
#' Default: \code{NULL}.
#' @param  n_foci number of focal points amongst the origin
#' locations. The origins to serve as focal
#' points are based on random selection. `n_foci` must be
#' smaller than `n_origins`.
#' @param foci_separation a value from `1` to `100`
#' indicating the nearness of focal points to one another.
#' A `0` separation indicates that focal points are in
#' close proximity
#' of one another, while a `100` indicates focal points being
#' evenly distributed across space.
#' @param conc_type concentration of the rest of the
#' origins (non-focal origins) around the focal ones. The options
#' are `"nucleated"` and `"dispersed"`.
#' @param p_ratio the smaller of the
#' two terms of proportional ratios.
#' For example, a value of \code{20}
#' implies \code{20:80} proportional ratios.
#' @param s_threshold defines the spatial
#' perception range of a walker at a given
#' location. Default: \code{250} (in the same
#' linear unit
#' as the `poly` - polygon shapefile).
#' @param step_length the maximum step taken
#' by a walker from one point to the next.
#' @param trend specifies the direction of the
#' long-term trend. Options are:
#' `"decreasing"`, `"stable"`,
#' and `"increasing"`. Default value is: `"stable"`.
#' @param slope slope of the long-term trend when
#' an `"increasing"` or `"decreasing"` trend is specified.
#' Options: `"gentle"` or `"steep"`. The default value is
#' set as \code{NULL} for the `stable` trend.
#' @param first_pDate date of the first seasonal peak of
#' the GTP (format: `"yyyy-mm-dd"`).
#' Default value is \code{NULL}, in which first seasonal
#' peak of 90 days is utilized.
#' seasonal cycle of 180 days is utilized (that is,
#' a seasonal cycle of 180 days).
#' @param show.plot (logical) Shows GTP.
#' Default is \code{FALSE}.
#' @param show.data (TRUE or FALSE) To show the output
#' data. Default is \code{FALSE}.
#' @param ... additional arguments to pass from
#' \code{gtp}, \code{walker} and \code{artif_spo}
#' functions.
#' @usage psim_artif(n_events=1000, start_date = "yyyy-mm-dd",
#' poly, n_origin, restriction_feat=NULL, field,
#' n_foci, foci_separation, conc_type = "dispersed",
#' p_ratio, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL, ..., show.plot=FALSE, show.data=FALSE)
#' @examples
#' \dontrun{
#'
#' #load boundary and land use of Camden
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' boundary = camden$boundary # get boundary
#' landuse = camden$landuse # get landuse
#'
#' #In this example, we will use a minimal number of
#' #'n_origin' (i.e. `20`) for faster computation:
#'
#' simulated_stpp <- psim_artif(n_events=200, start_date = "2021-01-01",
#' poly=boundary, n_origin=20, restriction_feat = NULL,
#' field = NULL,
#' n_foci=1, foci_separation = 10, conc_type = "dispersed",
#' p_ratio = 20, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL,show.plot=FALSE, show.data=FALSE)
#'
#' #If `n_events` is a vector of values,
#' #retrieve the simulated data for the
#' #corresponding vector element by using
#' #`simulated_stpp[[enter-element-index-here]]`, e.g.,
#' #to retrieve the first dataframe, use
#' #simulated_stpp[[1]].
#'
#' #The above example simulates point patterns on
#' #an unrestricted landscape. If set ,
#' #`restriction_feat = landuse` and
#' #`field = "restrVal"`, then the simulation
#' #is performed on a restricted landscape.
#' }
#'
#' @details
#' Both the walkers
#' and the landscape are configured arbitrarily (in accordance
#' with the users knowledge of the domain.
#' This function is computationally intensive. When run,
#' an estimate of the expected computational time
#' is first printed in the console for the user.
#' Argument with the largest impacts on the computational
#' time include `n_origin=50`, and `restriction_feat` when
#' not \code(NULL). Note: the `n_events`
#' argument has little of no impacts on the
#' computational time, and so it is recommended that
#' that a user inputs a vector of several values
#' to simulate.
#' Lastly, in addition to exporting the simulated
#' point patterns, the
#' function also returns the simulated origins,
#' the boundary and the restriction features
#' (if supplied).
#' @return Returns a list of artificial spatiotemporal
#' point patterns generated from scratch.
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @importFrom dplyr mutate bind_rows select
#' summarise left_join rename arrange
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom graphics points legend
#' @importFrom lubridate hms
#' @export
#'

psim_artif <- function(n_events=1000, start_date = "yyyy-mm-dd",
                       poly, n_origin, restriction_feat=NULL, field=NA,
                       n_foci,
                       foci_separation, conc_type = "dispersed",
                       p_ratio,
                       s_threshold = 50, step_length = 20,
                       trend = "stable",
                       first_pDate=NULL,
                       slope = NULL, ..., show.plot=FALSE, show.data=FALSE){

  #define global variables...
  nrowh <- origins <- NULL

  #first derive the spo object
  spo <- artif_spo(poly, n_origin =  n_origin, restriction_feat = restriction_feat,
                   n_foci=n_foci,
                   foci_separation = foci_separation,
                   conc_type = conc_type, p_ratio = p_ratio)

  #start_date <- as.Date(start_date)

  #check that start_date has value
  if(start_date == "yyyy-mm-dd"){
    stop("Error! 'start_date' argument has to be a real date!")
  }

  #check first peak value
  if(is.null(first_pDate)){
    first_pDate <- as.Date(start_date) + 90
  }

  output <- list()

  start_date <- as.Date(start_date)

  #global variables
    group_by <- idx <- . <- if_else <-
    tid <- NULL

  #define global variables
  x <- y <- NULL

  #get the poly
  poly <- spo$poly

  #test spo object class
  if(!spo$Class %in% c("artif_spo")){
    stop("The 'spo' object is NOT of correct 'artif_spo' Class!")
  }

  #next, extract from the spo, the N x 2 matrix or dataframe giving the
  #coordinates of the event origins (i.e. initial coordinates of the
  #simulation)
  coords <- spo$origins %>%
    select(x, y)

  #simulate the global temporal pattern
  gtp <- gtp(start_date = start_date, trend, slope=slope, first_pDate=first_pDate,
             show.plot=show.plot) #"01-01"


  #prepare date
  t1 <- as.Date(start_date)
  t <- seq(0, 365, by = 1)
  t2 <- t1 + t #list of dates

   #test polygon geometry
  if(!is.null(poly)){
    #-----
    poly_tester(poly)
    #-----
  }

  n = gtp$data#[1:4]

  #subset xy columns
  spo_xy <- as_tibble(spo$origins) %>%
    dplyr::select(x, y)

  #estimating computational time
  options(digits.secs = 5)
  tme1 <- Sys.time()
  event_loc_N <- lapply(n, function(n)
    stppSim::walker(n, s_threshold = s_threshold,
                    poly=poly, restriction_feat = restriction_feat,
                    field = field,
                    coords=as.vector(unlist(spo_xy[1,],)),
                    step_length = step_length ,
                    show.plot = FALSE)
  )
  tme2 <- Sys.time()
  #time_elapse <- tme2 - tme1
  time_elapse <- difftime(tme2,tme1,units = "secs")
  time_elapse <- round((time_elapse * n_origin)/60, digits=2)
  flush.console()
  time_elapse <- time_elapse + (time_elapse * 0.1)#add 10%
  cat("#=====")
  cat("The expected computational time for the process is:",paste(time_elapse, " minutes", sep=""),sep=" ")
  cat("=====#")

  #the actual process
  stp_All <- NULL

  for(b in seq_len(nrow(spo_xy))){ #b<-1
    event_loc_N <- lapply(n, function(n)
      stppSim::walker(n, s_threshold = s_threshold,
                      poly=poly, restriction_feat = restriction_feat,
                      field = field,
                      coords=as.vector(unlist(spo_xy[b,],)),
                      step_length = step_length ,
                      show.plot = FALSE)
    )

    loc_N <- rbindlist(event_loc_N,
                          use.names=TRUE, fill=TRUE, idcol="tid")

    loc_N <- loc_N %>%
      mutate(locid=b, prob=spo$origins$prob[b]) %>%
      mutate(time=format(((tid-1) + as.Date(start_date) + hms(time)),
                         "%Y-%m-%d %H:%M:%S"))%>%
      rename(datetime=time)

    stp_All <- stp_All %>%
      bind_rows(loc_N)
    }

  #generate all the results
  for(h in seq_len(length(n_events))){

    #add idx
    stp_All_ <- stp_All %>%
      rownames_to_column('ID') #%>% #add row as column

    #sample to derive required number
    samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events[h],
                                replace = FALSE, prob = stp_All_$prob)) #%>

    stp_All_ <- stp_All_[samp_idx, ]

    #sort
    stp_All_ <- stp_All_ %>%
      arrange(locid, tid, sn)

    output[h] <- list(stp_All_)
  }

  #add the origins
  output$origins <- spo$origins
  output$poly <- spo$poly
  output$resist <- restriction_feat

  return(output)
}
