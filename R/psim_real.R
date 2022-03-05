#' @include gtp.R
#' @include walker.R
#' @title Stpp from real (sample) origins
#' @description Generate spatiotemporal point pattern
#' from origins sampled based on real dataset.
#' @param n_events (integer) Number of points
#' (events) to simulate. Default: \code{1000}.
#' A vector of integer values can be supplied, in the
#' format `c(a1, a2, ....)`, where a1, a2, ...
#' represent different values.
#' @param ppt A 3-column matrix or list containing
#' `x` - eastings, `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd').
#' @param start_date Specifies the start date of
#' the sample data provided (format: `yyyy-mm-dd`).
#' If `NULL`, the earliest date
#' of the `t` field of `ppt` is utilized.
#' The end date is automatically set as the 365th day
#' from the start date.
#' @param poly (An sf or S4 object)
#' Spatial (administrative) boundary covering the area
#' under study. The default is `NULL`, in which an
#' arbitrary boundary is drawn to cover the spatial extent
#' of the data. The projection system of `poly` is assume
#' for `ppt`, therefore, a user needs to ensure that both
#' `poly` and `ppt`(-xy cordinates) are in the same
#' reference system for accurate result.
#' @param s_threshold (numeric) Spatial range
#' from the origin within
#' which a walker re-generate events.
#' Default: \code{NULL}, in which the value is
#' automatically estimated from the sample data (i.e., `ppt`).
#' @param n_origin (an integer) Number of
#' locations to serve as origins for walkers. The value has
#' largest impacts on the computational time.
#' @param resistance_feat (An S4 object) Optional
#' shapefile representing spaces across landscape
#' within which event
#' @param field A number in the range of \code{[0-1]}
#' (i.e. resistance values) to
#' assign to all features covered by `resistance_feat`; or
#' the name of a numeric field to extract such
#' resistance values for different feature classes.
#' The resistance value `0` and `1` indicate the
#' lowest and the highest restrictions, respectively,
#' to an event occuring within the space occupied
#' by a feature.
#' origins are not allowed. Default: \code{NULL}.
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio.
#' For example, a value of \code{20}
#' implies a \code{20:80} Pareto ratio.
#' @param crsys (string) The EPSG projection code that defines
#' the xy coordinates (of `ppt`). This will be utilized
#' if `poly` argument is \code{NULL}.
#' See "http://spatialreference.org/" for the list of
#' EPSG codes for different regions of the world.
#' As an example, the EPSG code for the British National Grid
#' projection system is: \code{"EPSG:27700"}.
#' @usage psim_real(n_events, ppt, start_date = NULL, poly = NULL,
#' s_threshold = NULL, n_origin=50, resistance_feat, field=NA,
#' p_ratio=20, crsys = NULL)
#' @examples
#' \dontrun{
#' data(camden_theft)
#' #specify the proportion of full data to use
#' sample_size <- 0.2
#' set.seed(1000)
#' dat_sample <- camden_theft[sample(1:nrow(camden_theft),
#' round((sample_size * nrow(camden_theft)), digits=0),
#' replace=FALSE),]
#' #plot(dat_sample$x, dat_sample$y) #preview
#' result <- psim_real(n_events=2000, ppt=dat_sample,
#' start_date = NULL, poly = NULL, s_threshold = NULL,
#' n_origin=50, resistance_feat, field=NA,
#' p_ratio=20, crsys = "EPSG:27700")
#' }
#' @details Returns an object of the class `real_spo`,
#' detailing the spatiotemporal properties of a real
#' sample dataset
#'
#' #' If not `NULL`, a numerical value
#' based on prior or expert knowledge is expected.
#'
#'
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
                      s_threshold = NULL,
                      n_origin=50, p_ratio=20, crsys = NULL){

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

  #length(which(stp_All_$OriginType == "Dominant"))
  #length(which(stp_All_$OriginType == "Non-dominant"))

  #-------------------------------------------
  #Temporal trend and patterns
  #stp_All_ %>%

  #create window to plot frou
  #spatial patterns
  # plot(stp_All_$x, stp_All_$y,
  #      main = "Spatial point distribution",
  #      xlab = "x",
  #      ylab = "y")
  #
  # #add origins
  # spo_forPlot <- st_properties$origins %>%
  #   mutate(pch = as.numeric(if_else(OriginType == "Dominant",
  #                  paste("20"), paste("1")))) #'20' is point type
  #
  # points(spo_forPlot$x, spo_forPlot$y,
  #        add=TRUE, pch=spo_forPlot$pch, col="red",
  #        cex=1.2)
  #
  # legend("bottomleft",
  #        legend = c("Events", "Origin (D)", "Origin (N)"),
  #             col = c("black","red","red"),
  #             pch = c(1, 20, 1))

  #temporal pattern
  #get t holder
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

  #combine and add as details
  #-------------------------------------------
  #add the origins
  output$origins <- st_properties$origins
  output$poly <- st_properties$poly
  output$resist <- resistance_feat
  return(output)

}
