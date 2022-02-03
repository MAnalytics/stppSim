#' @include gtp.R
#' @include walker.R
#' @title Point Pattern Simulation from real origins
#' @description Generate point pattern in space and time
#' from real sample dataset, based on the
#' temporal pattern and
#' spatial properties learnt from the sample dataset.
#' @param n_events (integer) Value of the total
#' number of points (events) to simulate. Default: \code{2000}.
#' A vector of integer values can also be inputted, such as
#' `c(a1, a2, ....)`, where a1, a2, ... represent different values.
#' @param ppt (matrix) coordinates (and time) vectors of points.
#' A 3-column matrix or list: `x` - eastings,
#' `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd'). The sample needs to
#' cover the one year period being investigated.
#' @param start_date (string) Specify the start date of
#' the sample point supplied (i.e. `ppt`),
#' in the format `yyyy-mm-dd`. If `NULL`, the earliest date
#' from the `ppt` is utilized. The end date is the 365th day
#' from the specified start date (by default).
#' @param poly (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). The boundary (spatial polygon) surrounding
#' the sample points. The default is `NULL` - meaning that an
#' arbitrary boundary is drawn to cover the spatial
#' point distribution. The 'poly' object must have a projection
#' system (crs) when not NULL.
#' @param crsys (string) The projection ('crs') system to utilize
#' when 'poly' argument is not NULL. You can obtain CRS string
#' from "http://spatialreference.org/". The `crs` can be set using
#' `proj4string(poly) <- "CRS string", where `CRS string` defines
#' the projection of `ppt`. When both `poly` and `crsys`
#' are not NULL, the function utilizes the crs of the former
#' @usage stp_learner(ppt, start_date = NULL, poly = NULL,
#' crsys = "CRS_string")
#' @examples
#' data(SanF_fulldata)
#' data(SanF_CRS_string)
#' #get a sample data
#' set.seed(1000)
#' sample_size <- 1000
#' dat_sample <- SanF_fulldata[sample(1:nrow(SanF_fulldata),
#' sample_size, replace=FALSE),]
#' stp_learner(dat_sample,
#' start_date = NULL, poly = NULL, crsys = SanF_CRS_string)
#' @details Returns an object of the class `real_spo`,
#' detailing the spatiotemporal properties of a real
#' sample dataset
#' @references https://www.google.co.uk/
#' @importFrom dplyr select group_by
#' mutate summarise left_join n arrange
#' desc
#' @importFrom tidyr replace_na
#' @importFrom sp SpatialPoints proj4string
#' @importFrom stats predict loess
#' @export

psim_artif <- function(ppt, n_events=2000, start_date = NULL, poly = NULL,
                       crsys = SanF_CRS_string){

  output <- list()

  st_properties <- stp_learner(ppt=dat_sample, start_date = start_date,
                               poly = poly, crsys = crsys)


  # #get the poly
  # poly <- spo$poly
  #
  # coords <- spo$origins %>%
  #   select(x, y)
  #
  # #simulate the global temporal pattern
  # gtp <- gtp(start_date=start_date, trend, slope=slope, first_s_peak=first_s_peak,
  #            show.plot=show.plot) #"01-01"
  #
  #
  #
  #
  #
  # n = gtp$data#[1:4]
  #
  # #to implement parallelizing later
  # no_of_clusters <- detectCores()
  #
  # #create clusters (use n-1 cores)
  # myCluster <- makeCluster((no_of_clusters-1), # number of cores to use
  #                          type = "PSOCK") # type of cluster
  #
  # #register cluster with foreach
  # registerDoParallel(myCluster)
  #
  # ##result <- foreach(x = c(4,9,16)) %dopar% sqrt(x)
  #
  # #subset xy columns
  # spo_xy <- spo$origins %>%
  #   select(x, y)
  #
  # #t1 <- Sys.time()
  #
  # pp_allTime <- foreach(idx = iter(spo_xy, by='row')) %dopar%
  #   lapply(n, function(n)
  #     stppSim::walker(n, s_threshold = s_threshold,
  #                     poly=poly, coords=as.numeric(as.vector(idx)),
  #                     step_length = 20,
  #                     show.plot = FALSE)
  #   )
  #
  # # t2 <- Sys.time()
  # # tme <- t2 - t1
  # # print(tme)
  #
  # #stop the cluster
  # stopCluster(myCluster)
  #
  #
  # length(pp_allTime)
  # #unlist the result..
  #
  # stp_All <- NULL
  #
  #
  # #combine all results by
  # for(loc in 1:length(spo$origins$OriginType)){ #loc<-1
  #   #extract slot 'intersection'
  #   p_events <- rbindlist(pp_allTime[[loc]],
  #                         use.names=TRUE, fill=TRUE, idcol="tid")
  #
  #   p_events <- p_events %>%
  #     mutate(locid=loc, prob=spo$origins$prob[loc],
  #            OriginType = spo$origins$OriginType[loc]) %>%
  #     #mutate(time=(tid-1) + as.Date(start_date))
  #
  #     mutate(time=format(((tid-1) + as.Date(start_date) + hms(time)),
  #                        "%Y-%m-%d %H:%M:%S"))%>%
  #     rename(datetime=time)
  #   stp_All <- stp_All %>%
  #     bind_rows(p_events)
  #
  # }
  #
  # #n_events <- c(2000, 3000)
  # #generate all the results
  # for(h in seq_len(length(n_events))){
  #
  #   #add idx
  #   stp_All_ <- stp_All %>%
  #     rownames_to_column('ID') #%>% #add row as column
  #
  #   #sample to derive required number
  #   samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events[h],
  #                                 replace = FALSE, prob = stp_All_$prob)) #%>
  #
  #   stp_All_ <- stp_All_[samp_idx, ]
  #
  #   output[h] <- list(stp_All_)
  # }

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