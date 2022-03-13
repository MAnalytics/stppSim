#' @include gtp.R
#' @include walker.R
#' @title Stpp from real (sample) origins
#' @description Generates spatiotemporal point pattern
#' from origins sampled based on real sample dataset.
#' @param n_events number of points
#' (events) to simulate. Default: \code{1000}.
#' A vector of integer values can be supplied, such as,
#' c(`a`1, `a`2, ....)`, where `a`1, `a`2, ...
#' represent different integer values.
#' @param ppt A 3-column matrix or list containing
#' `x` - eastings, `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd').
#' @param start_date the start date of the temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The temporal pattern will normally cover
#' 1-year period.
#' @param poly (An sf or S4 object)
#' a polygon shapefile defining the extent of the landscape
#' @param s_threshold defines the spatial
#' perception range of a walker at a given
#' location. Default: \code{250} (in the same
#' linear unit
#' as the `poly` - polygon shapefile).
#' @param step_length the maximum step taken
#' by a walker from one point to the next.
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
#' @param p_ratio the smaller of the
#' two terms of proportional ratios.
#' For example, a value of \code{20}
#' implies \code{20:80} proportional ratios.
#' @param crsys (string) the EPSG code of the projection
#' system of the `ppt` coordinates. This only used if
#' `poly` argument is \code{NULL}.
#' See "http://spatialreference.org/" for the list of
#' EPSG codes for different regions of the world.
#' As an example, the EPSG code for the British National Grid
#' projection system is: \code{"EPSG:27700"}.
#' @usage psim_real(n_events, ppt, start_date = NULL, poly = NULL,
#' s_threshold = NULL, step_length = 20, n_origin=50,
#' resistance_feat, field=NA,
#' p_ratio=20, crsys = NULL)
#' @examples
#' \dontrun{
#' data(camden_crimes)
#' #subset 'theft' crime
#' theft <- camden_crimes[which(camden_crimes$type ==
#' "Theft"),]
#' #specify the proportion of full data to use
#' sample_size <- 0.2
#' set.seed(1000)
#' dat_sample <- theft[sample(1:nrow(theft),
#' round((sample_size * nrow(theft)), digits=0),
#' replace=FALSE),1:3]
#' #plot(dat_sample$x, dat_sample$y) #preview
#' result <- psim_real(n_events=2000, ppt=dat_sample,
#' start_date = NULL, poly = NULL, s_threshold = NULL,
#' step_length = 20, n_origin=50, resistance_feat, field=NA,
#' p_ratio=20, crsys = "EPSG:27700")
#' }
#' @details
#' The movement characteristics of walkers as well
#' as the configuration of the landscape are defined
#' based on the properties learnt from the real sample
#' data. The explanations under `psim_artif`
#' function regarding the computation time
#' apply.
#' @references
#' Davies, T.M. and Hazelton, M.L. (2010), Adaptive
#' kernel estimation of spatial relative risk,
#' Statistics in Medicine, 29(23) 2423-2437.
#' Terrell, G.R. (1990), The maximal smoothing principle
#' in density estimation, Journal of the
#' American Statistical Association, 85, 470-477.
#' @importFrom dplyr select group_by
#' mutate summarise left_join n arrange
#' desc
#' @importFrom tidyr replace_na
#' @importFrom sp SpatialPoints proj4string
#' @importFrom stats predict loess
#' @export

psim_real <- function(n_events, ppt, start_date = NULL, poly = NULL,#
                      s_threshold = NULL, step_length = 20,
                      n_origin=50, resistance_feat,
                      field = NA, p_ratio=20, crsys = NULL){

  idx <- tid <- x <- y <- if_else <- t2 <-
    axis <- . <- OriginType <- NULL

  output <- list()

  st_properties <- stp_learner(ppt=ppt, start_date = start_date,
                               poly = poly, n_origin=50,
                               p_ratio = p_ratio, crsys = crsys)
  #return start_date

  #names(st_properties)

  #test polygon geometry
  # #get the poly
  if(!is.null(poly)){
    #-----
    poly_tester(poly)
    #-----
  } else {

    poly <- st_properties$poly

  }

  #get coordinates
  coords <- st_properties$origins %>%
    select(x, y)

  #get the global temporal pattern
  n = st_properties$gtp#[1:4]
  #
  #to implement parallelizing later
  no_of_clusters <- detectCores()

  #create clusters (use n-1 cores)
  myCluster <- makeCluster((no_of_clusters-1), # number of cores to use
                           type = "PSOCK") # type of cluster

  #register cluster with foreach
  registerDoParallel(myCluster)

  ##result <- foreach(x = c(4,9,16)) %dopar% sqrt(x)

  #subset xy columns
  spo_xy <- st_properties$origins %>%
    select(x, y)

  #t1 <- Sys.time()

  if(is.null(s_threshold)){
  pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
    lapply(n, function(n)
      stppSim::walker(n, s_threshold = st_properties$s_threshold, #jsut example for now..
                      poly=poly,resistance_feat = resistance_feat,
                      field = field,
                      coords=as.numeric(as.vector(idx)),
                      step_length = step_length,
                      show.plot = FALSE)
    )}

  if(!is.null(s_threshold)){
  pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
    lapply(n, function(n)
      stppSim::walker(n, s_threshold = s_threshold, #jsut example for now..
                      poly=poly, resistance_feat = resistance_feat,
                      field = field,
                      coords=as.numeric(as.vector(idx)),
                      step_length = step_length,
                      show.plot = FALSE)
    )}
  #)
  #t2 <- Sys.time()
  #tme <- t2 - t1
  #print(tme)

  #stop the cluster
  stopCluster(myCluster)

  #
  length(pp_allTime)
  #unlist the result..

  stp_All <- NULL

  #combine all results by
  for(loc in 1:nrow(st_properties$origins)){ #loc<-1
    #extract slot 'intersection'
    p_events <- rbindlist(pp_allTime[[loc]],
                          use.names=TRUE, fill=TRUE, idcol="tid")

    p_events <- p_events %>%
      mutate(locid=loc, prob=st_properties$origins$prob[loc])%>%#,
             #OriginType = st_properties$origins$OriginType[loc]) %>%
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


  #combine and add as details
  #-------------------------------------------
  #add the origins
  output$origins <- st_properties$origins
  output$poly <- st_properties$poly
  output$resist <- resistance_feat
  return(output)

}
