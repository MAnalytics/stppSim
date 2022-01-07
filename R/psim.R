#' @title Modeling of the Global Temporal Pattern
#' @description Models the global temporal pattern (of
#' the point process) as consisting of the global linear
#' trend and the seasonality.
#' @param n (integer) Total Number of events to simulate.
#' @param spo (a list or dataframe) A list of spatial boundary
#' coordinates (or shapefile) within which the events are confined.
#' Should be generated using `random_spo` or `constrained_spo`
#' function.
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
#' Default:\code{"daily"}. Other values are:
#' \code{"weekly", "monthly"}.
#' @param st_skewness (numeric) The tightness of events in space and time.
#' The value ranges from \code{0 - 1}, with event
#' volume being skewed towards the dominant origins, as the value tends
#' to \code{1}. Default: \code{0.5}. This index also controls the
#' total volume of events across space and time.
#' @param step_length (numeric) A maximum step taken at a time
#' by a walker from one state to the next. Should be a fraction
#' of the spatial units of the landscape. Default: half the size
#' of the minimum spatial unit in a landscape
#' (for a constraint landscape) or
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon defining the boundary
#' within which events are to be generated.
#' @param show.data (TRUE or FALSE) To show the output data
#' Default is \code{FALSE}.
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
#' @param npoints (an integer) Number of origins (points) to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
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
#' @importFrom dplyr mutate bind_rows
#' @export
#'

psim <- function(n, spo, start_date, s_threshold=50, st_skewness = 0.5,
                 step_length = 20, poly=camden_boundary, show.data = TRUE,
                 trend, slope, first_s_peak, npoints, p_ratio){

  #test for n value
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
  gtp <- gtp(start_date = "01-01", trend = "stable",
      slope = "NULL", first_s_peak=90, show.plot =FALSE)


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
  for(loc in 1:length(spo$origins$OriginType)){ #loc<-2
    pp_allTime <- lapply(n, function(n)
      walker(n, s_threshold = s_threshold,
             poly=poly, coords=c(spo$origins$x[loc],spo$origins$y[loc]),
                      step_length = 20,
                      show.plot = FALSE))

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
  samp_idx <- as.numeric(sample(stp_All_$ID, size = n, replace = FALSE, prob = stp_All_$prob)) #%>

  stp_All_ <- stp_All_[samp_idx, ]

  #length(which(stp_All_$OriginType == "Dominant"))
  #length(which(stp_All_$OriginType == "Non-dominant"))

}
