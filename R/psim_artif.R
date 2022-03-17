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
#' poly, n_origin, restriction_feat, field,
#' n_foci, foci_separation, conc_type = "dispersed",
#' p_ratio, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL, ..., show.plot=FALSE, show.data=FALSE)
#' @examples
#' \dontrun{
#' require(rgdal)
#' path <- system.file("extdata", package="stppSim")
#' camden_boundary <- readOGR(dsn=path, layer = "camden_boundary", verbose=FALSE)
#' landuse <- system.file("extdata",
#' "landuse.rda", package="stppSim")
#' simulated_stpp <- psim_artif(n_events=200, start_date = "2021-01-01",
#' poly=camden_boundary, n_origin=50, restriction_feat = landuse,
#' field = "rValue1",
#' n_foci=5, foci_separation = 10, conc_type = "dispersed",
#' p_ratio = 20, s_threshold = 50, step_length = 20,
#' trend = "stable", first_pDate=NULL,
#' slope = NULL,show.plot=FALSE, show.data=FALSE)
#' #If `n_events` is a vector, access the corresponding
#' #simulated data for each vector entry using
#' #`simulated_stpp[[vector-index]]`
#' }
#' @details
#' Generates spatiotemporal point pattern
#' based on the interactions between the
#' walkers (agents) and the landscape. Both the walkers
#' and the landscape are configured arbitrarily (in accordance
#' with the users knowledge of the domain under study.
#' This function is computationally intensive, and so
#' has been implemented to use parallel processing for
#' faster results. An `n`-1 cores on a PC is utilized
#' for the simulation. The argument `n_origin` has the
#' largest impacts on the computation. The computational time
#' is `15` minutes for the example above on a (4-1)
#' core laptop (with parameters setting: `n_origin=50` and
#' `restriction_feat = NULL`). The computational time
#' increases to 2hours when
#' `restriction_feat = landuse`). Note: the `n_events`
#' argument has little of no impacts on the
#' computational time.
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

psim_artif <- function(n_events=1000, start_date = "yyyy-mm-dd",
                       poly, n_origin, restriction_feat, field=NA,
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
         poly=poly, restriction_feat = restriction_feat,
         field = field,
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
  output$resist <- restriction_feat

  return(output)
}
