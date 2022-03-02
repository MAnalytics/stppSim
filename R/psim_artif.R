#' @include gtp.R
#' @include walker.R
#' @title Point Pattern Simulation from artificial origins
#' @description Generate point pattern in space and time
#' from scratch based on a specified temporal pattern and
#' spatial properties.
#' @param n_events (integer) Value of the total
#' number of points (events) to simulate. Default: \code{2000}.
#' A vector of integer values can also be inputted, such as
#' `c(a1, a2, ....)`, where a1, a2, ... represent different values.
#' @param start_date (Date) The start date of simulation. The date should
#' be in the format `"yyyy-mm-dd"`. A specified date can be earlier or later
#' than this stated default value. By default, a 1-year worth of
#' date is simulated. In other words, the end date of
#' simulation is the next 365th day
#' from the specified start date.
#' @param poly (A dataframe or S4 object) A dataframe of X, Y
#' coordinates or a spatial boundary (as "SpatialPolygonsDataFrame",
#' "SpatialPolygons", or "sf") representing the boundary within which
#' events are to be generated.
#' @param n_origin (an integer) Number of origins to simulate.
#' Default:\code{50}. This is the parameter that has the greatest
#' influence on the computational time.
#' @param space_resist ("SpatialPolygonsDataFrame",
#' "SpatialPolygons", or "sf") Optional features showing
#' spaces across landscape within which spatial
#' points (origins) are not allowed. Default: \code{NULL}.
#' @param  n_foci (an integer) A value indicating the number of
#' focal points amongst event origins.
#' @param foci_separation (an integer) A value between `0` and `10`
#' indicating the nearness of focal points from one another. A `0`
#' separation indicates all focal points located in a close proximity
#' while `10` indicates focal points that are evenly distributed across
#' space.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
#' @param p_ratio (an integer) The smaller of the
#' two terms of the Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param trend (string) Specify the trend direction of
#' the GTP. Values are: `"decreasing"`, `"stable"`,
#' and `"increasing"`. Default is: `"stable"`.
#' @param first_pDate (in `"yyyy-mm-dd"` format).
#' Date of the
#' first seasonal peak of the time series.
#' Default value is \code{NULL}, in which a
#' seasonal cycle of 180 days is utilized. That is,
#' a first seasonal peak of 90 days.
#' @param slope (string) Slope GTP trend if
#' "increasing" or "decreasing" trend is specified.
#' Values: `"gentle"` or `"steep"`.
#' Default value is \code{NULL} (i.e., for `stable` trend).
#' @param show.plot (TRUE or FALSE) Whether to display
#' the plots after execution.
#' @param show.data (TRUE or FALSE) To show the output data
#' Default is \code{FALSE}.
#' @param ... additional arguments to pass from
#' \code{gtp} and \code{walker} functions. Arguments from
#' \code{gtp} can be used to define the nature of the
#' temporal trend and pattern over time, while arguments
#' from \code{walker} can be utilized to define the properties
#' of event generators (walkers) across the landscape.
#' @examples
#' @details A list containing `k` entries,
#' where `k` is the length of `n_events` (see above).
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @importFrom dplyr mutate bind_rows select
#' summarise left_join rename
#' @importFrom tibble rownames_to_column
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators iter
#' @importFrom graphics points legend
#' @importFrom lubridate hms
#' @export
#'

psim_artif <- function(n_events=2000, start_date = "yyyy-mm-dd",
                       poly, n_origin, space_resist = space_resist,
                       n_foci,
                       foci_separation, p_ratio,
                       s_threshold = 50,
                       trend = "stable",
                       first_pDate=NULL,
                       slope = NULL, ..., show.plot=FALSE, show.data=FALSE){

  #define global variables...
  nrowh <- NULL

  #first derive the spo object
  spo <- artif_spo(poly, n_origin =  n_origin, space_resist = space_resist,
                   n_foci=5,
                   foci_separation = foci_separation, p_ratio = p_ratio)


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
  # first_pDate <- poly <- show.plot <- slope <-
  #   trend <- start_date <- OriginType <-
  #   axis <-
    group_by <- idx <- . <- if_else <-
    tid <- NULL

  #test for n_event value
  #-----------------------------

  #define global variables
  x <- y <- NULL

  #spo <- random_spo(poly, npoints=5, p_ratio, show.plot=TRUE)


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
  gtp <- gtp(start_date=start_date, trend, slope=slope, first_pDate=first_pDate,
             show.plot=show.plot) #"01-01"


  #prepare date
  t1 <- as.Date(start_date)
  t <- seq(0, 365, by = 1)
  t2 <- t1 + t #list of dates

  # if(is.null(poly)){
  #
  # }

  #test polygon geometry
  if(!is.null(poly)){
    #-----
    poly_tester(poly)
    #-----
  }

  #check if spo and poly covers the same location (or overlap
  #each other)


  #spo <- spo[2,]

  n = gtp$data#[1:4]

  #to implement parallelizing later
  no_of_clusters <- detectCores()

  #create clusters (use n-1 cores)
  myCluster <- makeCluster((no_of_clusters-1), # number of cores to use
                           type = "PSOCK") # type of cluster

  #register cluster with foreach
  registerDoParallel(myCluster)

  ##result <- foreach(x = c(4,9,16)) %dopar% sqrt(x)

  #subset xy columns
  spo_xy <- spo$origins %>%
    select(x, y)

  #t1 <- Sys.time()

  pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
    lapply(n, function(n)
    stppSim::walker(n, s_threshold = s_threshold,
         poly=poly, coords=as.numeric(as.vector(idx)),
                  step_length = 20,
                  show.plot = FALSE)
    )

  # t2 <- Sys.time()
  # tme <- t2 - t1
  # print(tme)

  #stop the cluster
  stopCluster(myCluster)


  length(pp_allTime)
  #unlist the result..

  stp_All <- NULL


  #combine all results by
  for(loc in 1:nrowh(spo$origins)){ #loc<-1
    #extract slot 'intersection'
    p_events <- rbindlist(pp_allTime[[loc]],
                            use.names=TRUE, fill=TRUE, idcol="tid")

    p_events <- p_events %>%
      mutate(locid=loc, prob=spo$origins$prob[loc],
           OriginType = spo$origins$OriginType[loc]) %>%
      #mutate(time=(tid-1) + as.Date(start_date))

      mutate(time=format(((tid-1) + as.Date(start_date) + hms(time)),
                         "%Y-%m-%d %H:%M:%S"))%>%
      rename(datetime=time)
  stp_All <- stp_All %>%
    bind_rows(p_events)

  }

#n_events <- c(2000, 3000)
  #generate all the results
  for(h in seq_len(length(n_events))){

    #add idx
    stp_All_ <- stp_All %>%
      rownames_to_column('ID') #%>% #add row as column

    #sample to derive required number
    samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events[h],
                                replace = FALSE, prob = stp_All_$prob)) #%>

    stp_All_ <- stp_All_[samp_idx, ]

    output[h] <- list(stp_All_)
  }

  #length(which(stp_All_$OriginType == "Dominant"))
  #length(which(stp_All_$OriginType == "Non-dominant"))

  # #-------------------------------------------
  # #Temporal trend and patterns
  # #stp_All_ %>%
  #
  # #create window to plot frou
  # #spatial patterns
  # plot(stp_All_$x, stp_All_$y,
  #      main = "Spatial point distribution",
  #      xlab = "x",
  #      ylab = "y")
  #
  # #add origins
  # spo_forPlot <- spo$origins %>%
  #   mutate(pch = as.numeric(if_else(OriginType == "Dominant",
  #                        paste("20"), paste("1"))))
  #
  # points(spo_forPlot$x, spo_forPlot$y,
  #        add=TRUE, pch=spo_forPlot$pch, col="red",
  #        cex=1.2)
  #
  # legend("bottomleft",
  #        legend = c("Events", "Origin (D)", "Origin (N)"),
  #             col = c("black","red","red"),
  #             pch = c(1, 20, 1))
  #
  # #temporal pattern
  # #get t holder
  # all_t <- data.frame(tid=unique(stp_All$tid))
  #
  # temp_p <- stp_All_ %>%
  #   group_by(tid) %>%
  #   summarise(ct = n())
  #
  # temp_pattern <- all_t %>%
  #   left_join(temp_p)%>%
  #   replace(is.na(.), 0)
  #
  # plot(temp_pattern$tid, temp_pattern$ct, 'l', xaxt = "n")
  #
  # ticks <- seq(temp_pattern$tid[1],
  #              temp_pattern$tid[length(temp_pattern$tid)])
  # ix <- seq(temp_pattern$tid[1],
  #           temp_pattern$tid[length(temp_pattern$tid)], by=30)#every 60 days
  #
  # dates_list <- t2[ix]
  # ticks <- ticks[ix]
  # axis(1, at = ticks, labels = dates_list, tcl = -0.2)
  #
  # #Resulting global spatial bandwidth
  #
  # #Resulting global temporal bandwidth
  #
  # #combine and add as details
  #@data
  #-------------------------------------------
  return(output)

}
