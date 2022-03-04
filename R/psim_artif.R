#' @include gtp.R
#' @include walker.R
#' @title Artificial spatiotemporal point pattern
#' @description Generate artificial spatiotemporal
#' point pattern from synthesized origins.
#' @param n_events (integer) Number of points
#' (events) to simulate. Default: \code{2000}.
#' A vector of integer values can be supplied, in the
#' format `c(a1, a2, ....)`, where a1, a2, ...
#' represent different values.
#' @param start_date The start date of temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The GTP will usually covers a 1-year period.
#' @param poly (An sf or S4 object)
#' A polygon shapefile within which
#' event origins are to be situated.
#' @param n_origin (an integer) Value specifying
#' the number of event origins to synthetize.
#' Default:\code{50}. Value specified here has the greatest
#' influence on the computational time.
#' @param resistance_feat (An S4 object) Optional
#' shapefile representing spaces across landscape
#' within which event
#' origins are not allowed. Default: \code{NULL}.
#' @param  n_foci (an integer) Value indicating the number of
#' focal points amongst event origins. `n_foci` will usually be
#' smaller than `n_origin`.
#' @param foci_separation (an integer) A percentage value indicating
#' indicating the nearness of focal points from one another. A `0`
#' separation indicates that focal points are in close proximity
#' of one another, while a `100` indicates focal points being
#' evenly distributed across space.
#' @param conc_type (string) Specifies the spatial pattern
#' of non-focal origin (strengths) in relation to
#' to their nearest focal origins. Value is either
#' \code{"nucleated"} or \code{"dispersed"}.
#' @param p_ratio (an integer) The smaller of the
#' two terms of the Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param trend (string) Specify the trend direction of
#' the GTP. Values are: `"decreasing"`, `"stable"`,
#' and `"increasing"`. Default is: `"stable"`.
#' @param slope (string) Slope GTP trend if
#' "increasing" or "decreasing" trend is specified.
#' Values: `"gentle"` or `"steep"`.
#' Default value is \code{NULL} (i.e., for `stable` trend).
#' @param first_pDate (in `"yyyy-mm-dd"` format).
#' Date of the
#' first seasonal peak of the time series.
#' Default value is \code{NULL}, in which a
#' seasonal cycle of 180 days is utilized. That is,
#' a first seasonal peak of 90 days.
#' @param show.plot (logical) Shows GTP.
#' Default is \code{FALSE}.
#' @param s_threshold (numeric) Spatial threshold
#' value. This is the spatial range within which
#' a walker perceives it's environment at any
#' instant. Default: \code{250} (in the same linear unit
#' as the `poly` - polygon shapefile).
#' @param step_length (numeric) A maximum step taken at a time
#' by a walker from one state to the next.
#' @param show.data (TRUE or FALSE) To show the output
#' data. Default is \code{FALSE}.
#' @param ... additional arguments to pass from
#' \code{gtp}, \code{walker} and \code{artif_spo}
#' functions.
#' @usage psim_artif(n_events=2000, start_date = "yyyy-mm-dd",
#' poly, n_origin, resistance_feat,
#' n_foci, foci_separation, conc_type = "dispersed",
#' p_ratio, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL, ..., show.plot=FALSE, show.data=FALSE)
#' @examples
#' \dontrun{
#' data(camden_boundary)
#' data(camden_landuse)
#' artif_stpp <- psim_artif(n_events=200, start_date = "2021-01-01",
#' poly=camden_boundary, n_origin=50,, resistance_feat = camden_landuse,
#' n_foci=5, foci_separation = 10, conc_type = "dispersed",
#' p_ratio = 20, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL,show.plot=FALSE, show.data=FALSE)
#' }
#' @details
#' Produces artificial spatiotemporal point
#' patterns based on the behaviours of a set of
#' `walkers` within a configured landscape. See the
#' `walker` function on how to define the properties of
#' a walker, and see `gtp` and `artif_spo`
#' functions on how to configure the temporal and spatial
#' domain within which walkers operate.
#' @return Returns a list of artificial spatiotemporal
#' point patterns.
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
                       poly, n_origin, resistance_feat,
                       n_foci,
                       foci_separation, conc_type = "dispersed",
                       p_ratio,
                       s_threshold = 50, step_length = 20,
                       trend = "stable",
                       first_pDate=NULL,
                       slope = NULL, ..., show.plot=FALSE, show.data=FALSE){

  #define global variables...
  nrowh <- NULL

  #first derive the spo object
  spo <- artif_spo(poly, n_origin =  n_origin, resistance_feat = resistance_feat,
                   n_foci=n_foci,
                   foci_separation = foci_separation,
                   conc_type = conc_type, p_ratio = p_ratio)

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

  #to implement parallelizing later
  no_of_clusters <- detectCores()

  #create clusters (use n-1 cores)
  myCluster <- makeCluster((no_of_clusters-1), # number of cores to use
                           type = "PSOCK") # type of cluster

  #register cluster with foreach
  registerDoParallel(myCluster)

  #subset xy columns
  spo_xy <- spo$origins %>%
    select(x, y)

  #tme1 <- Sys.time()

  #simulate walkers
  pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
    lapply(n, function(n)
    stppSim::walker(n, s_threshold = s_threshold,
         poly=poly, resistance_feat = resistance_feat,
         coords=as.numeric(as.vector(idx)),
                  step_length = step_length ,
                  show.plot = FALSE)
    )

  # tme2 <- Sys.time()
  # tme <- tme2 - tme1
  # print(tme)
  #stop the cluster
  stopCluster(myCluster)

  length(pp_allTime)
  #unlist the result..

  stp_All <- NULL

  #combine all results by
  for(loc in 1:nrow(spo$origins)){ #loc<-1
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

  #add the origins
  output$origins <- spo$origins
  output$poly <- spo$poly
  output$resist <- resistance_feat

  return(output)
}
