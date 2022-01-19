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
#' function. The `spo$poly` output is set as `poly` argument
#' in this function.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
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
#' summarise left_join
#' @importFrom tibble rownames_to_column
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators iter
#' @importFrom graphics points legend
#'
#' @export
#'

psim <- function(n_events=2000, spo, s_threshold = 50, st_skewness = 0.5, ...,
                 show.data=FALSE){

  #global variables
  first_s_peak <- poly <- show.plot <- slope <-
    trend <- start_date <- OriginType <- axis <-
    group_by <- idx <- . <- if_else <-
    tid <- NULL

  #test for n_event value
  #-----------------------------

  #-----------------------------

  #define global variables
  x <- y <- NULL

  #spo <- random_spo(poly, npoints=50, p_ratio, show.plot=TRUE)#deal with showing plot later

  #get the poly
  poly <- spo$poly

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
  gtp <- gtp(start_date=start_date, trend=trend, slope=slope, first_s_peak=first_s_peak,
             show.plot=show.plot) #"01-01"


  #prepare date
  t1 <- as.Date(paste("2021", start_date, sep="-"))
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

  t1 <- Sys.time()

  pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
    lapply(n, function(n)
    stppSim::walker(n, s_threshold = s_threshold,
         poly=poly, coords=as.numeric(as.vector(idx)),
                  step_length = 20,
                  show.plot = FALSE)
    )

  t2 <- Sys.time()
  tme <- t2 - t1
  print(tme)

  #stop the cluster
  stopCluster(myCluster)


  length(pp_allTime)
  #unlist the result..

  stp_All <- NULL


  #combine all results by
  for(loc in 1:length(spo$origins$OriginType)){ #loc<-1
    #extract slot 'intersection'
    p_events <- rbindlist(pp_allTime[[loc]],
                            use.names=TRUE, fill=TRUE, idcol="tid")

    p_events <- p_events %>%
      mutate(locid=loc, prob=spo$origins$prob[loc],
           OriginType = spo$origins$OriginType[loc]) #%>%

  stp_All <- stp_All %>%
    bind_rows(p_events)

  }

  #add idx
  stp_All_ <- stp_All %>%
    rownames_to_column('ID') #%>% #add row as column

  #sample to derive required number
  samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events,
                                replace = FALSE, prob = stp_All_$prob)) #%>

  stp_All_ <- stp_All_[samp_idx, ]

  #length(which(stp_All_$OriginType == "Dominant"))
  #length(which(stp_All_$OriginType == "Non-dominant"))

  #-------------------------------------------
  #Temporal trend and patterns
  #stp_All_ %>%

  #spatial patterns
  plot(stp_All_$x, stp_All_$y,
       main = "Spatial point distribution",
       xlab = "x",
       ylab = "y")

  #add origins
  spo_forPlot <- spo$origins %>%
    mutate(pch = as.numeric(if_else(OriginType == "Dominant",
                         paste("20"), paste("1"))))

  points(spo_forPlot$x, spo_forPlot$y,
         add=TRUE, pch=spo_forPlot$pch, col="red",
         cex=1.2)

  legend("bottomleft",
         legend = c("Events", "Origin (D)", "Origin (N)"),
              col = c("black","red","red"),
              pch = c(1, 20, 1))

  #temporal pattern
  #get t holder
  all_t <- data.frame(tid=unique(stp_All$tid))

  temp_p <- stp_All_ %>%
    group_by(tid) %>%
    summarise(ct = n())

  temp_pattern <- all_t %>%
    left_join(temp_p)%>%
    replace(is.na(.), 0)

  plot(temp_pattern$tid, temp_pattern$ct, 'l', xaxt = "n")

  ticks <- seq(temp_pattern$tid[1],
               temp_pattern$tid[length(temp_pattern$tid)])
  ix <- seq(temp_pattern$tid[1],
            temp_pattern$tid[length(temp_pattern$tid)], by=30)#every 60 days

  dates_list <- t2[ix]
  ticks <- ticks[ix]
  axis(1, at = ticks, labels = dates_list, tcl = -0.2)

  #Resulting global spatial bandwidth

  #Resulting global temporal bandwidth

  #combine and add as details
  #@data
  #-------------------------------------------
  return(stp_All_)

}
