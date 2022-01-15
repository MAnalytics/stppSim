#' @include gtp.R
#' @include walker.R
#' @title Modeling of the Global Temporal Pattern
#' @description Models the global temporal pattern (of
#' the point process) as consisting of the global linear
#' trend and the seasonality.
#' @param n_events (integer) Total Number of events to simulate.
#' Default: \code{1000}.
#' @param spo (a list or dataframe) A list of spatial boundary
#' coordinates (or shapefile) within which the events are confined.
#' Should be generated using `random_spo` or `constrained_spo`
#' function.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
#' Default:\code{"daily"}. Other values are:
#' \code{"weekly", "monthly"}.
#' @param st_skewness (numeric) The tightness of events in space and time.
#' The value ranges from \code{0 - 1}, with event
#' volume being skewed towards the dominant origins, as the value tends
#' to \code{1}. Default: \code{0.5}. This index also controls the
#' total volume of events across space and time.
#' @param show.data (TRUE or FALSE) To show the output data
#' Default is \code{FALSE}.
#' @param ... additional arguments to pass from
#' \code{gtp} and \code{walker} functions. Arguments from
#' \code{gtp} can be used to define the nature of the
#' temporal trend and pattern over time, while arguments
#' from \code{walker} can be utilized to define the properties
#' of event generators (walkers) across the landscape.
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @importFrom dplyr mutate bind_rows select
#' @importFrom tibble rownames_to_column
#' @export
#'

psim <- function(n_events=1000, spo, s_threshold = 50, st_skewness = 0.5, ...,
                 show.data=FALSE){

  #global variables
  first_s_peak <- poly <- show.plot <- slope <-
    trend <- start_date <- NULL

  #test for n_event value
  #-----------------------------

  #-----------------------------

  #define global variables
  x <- y <- NULL

  #spo <- random_spo(poly, npoints=50, p_ratio, show.plot=TRUE)#deal with showing plot later

  #test spo object class
  if(spo$Class != "spo"){
    stop("The 'spo' object is NOT an 'spo' Class!")
  }

  #next, extract from the spo, the N x 2 matrix or dataframe giving the
  #coordinates of the event origins (i.e. initial coordinates of the
  #simulation)
  coords <- spo$origins %>%
    select(x, y)

  #simulate the global temporal pattern
  gtp <- gtp(start_date="01-01", trend="stable", slope="NULL", first_s_peak=90,
             show.plot=TRUE)


  #-----
  poly_tester(poly)
  #-----

  #check if spo and poly covers the same location (or overlap
  #each other)


  #spo <- spo[2,]

  n = gtp$data

  stp_All <- NULL

  #to implement parallelizing later

  #loop though each location and simulate point
  for(loc in 1:length(spo$origins$OriginType)){ #loc<-1
    pp_allTime <- lapply(n, function(n)
      walker(n, s_threshold = s_threshold,
             poly=poly, coords=c(spo$origins$x[loc],spo$origins$y[loc]),
                      step_length = 20,
                      show.plot = FALSE)
      )

    #collapse list
    pp_allTime <- rbindlist(pp_allTime,
                            use.names=TRUE, fill=TRUE, idcol="tid")

    # if(loc==1){
    #   bk_ <- pp_allTime
    # }

    #append location id
    pp_allTime <- pp_allTime %>%
      mutate(locid=loc, prob=spo$origins$prob[loc],
             OriginType = spo$origins$OriginType[loc]) #%>%
      #append location id and pareto prob
      #mutate(x = spo$origins$x[loc] + x, y = spo$origins$y[loc] + y)#update coordinates

    stp_All <- stp_All %>%
      bind_rows(pp_allTime)


    flush.console()
    print(loc)
  }


  #length(which(stp_All$OriginType == "Dominant"))
  #length(which(stp_All$OriginType == "Non-dominant"))


  #spatial and temporal tightness
  stp_All_ <- stp_All %>%
    rownames_to_column('ID') #%>% #add row as column

  #sample
  samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events, replace = FALSE, prob = stp_All_$prob)) #%>

  stp_All_ <- stp_All_[samp_idx, ]

  #length(which(stp_All_$OriginType == "Dominant"))
  #length(which(stp_All_$OriginType == "Non-dominant"))

  #-------------------------------------------
  #Temporal trend and patterns
  #stp_All_ %>%

  #spatial patterns

  #optimal spatial bandwidth

  #optimal temporal bandwidth

  #combine and add as details
  #@data
  #-------------------------------------------
  return(stp_All_)

}
