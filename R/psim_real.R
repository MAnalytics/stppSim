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
#' restriction_feat=NULL, field=NA,
#' p_ratio=20, crsys = NULL)
#' @examples
#' \dontrun{
#' data(camden_crimes)
#' #subset 'theft' crime
#' theft <- camden_crimes[which(camden_crimes$type ==
#' "Theft"),]
#'
#' #specify the proportion of full data to use
#' sample_size <- 0.2
#' set.seed(1000)
#' dat_sample <- theft[sample(1:nrow(theft),
#' round((sample_size * nrow(theft)), digits=0),
#' replace=FALSE),1:3]
#' #plot(dat_sample$x, dat_sample$y) #preview
#'
#' #load boundary and land use of Camden
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' landuse = camden$landuse # get landuse
#'
#' #simulate data
#' simulated_stpp <- psim_real(n_events=2000, ppt=dat_sample,
#' start_date = NULL, poly = NULL, s_threshold = NULL,
#' step_length = 20, n_origin=20,
#' restriction_feat = NULL, field=NULL,
#' p_ratio=20, crsys = "EPSG:27700")
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
#' The movement characteristics of walkers as well
#' as the configuration of the landscape are defined
#' based on the properties learnt from the real sample
#' data. See under `psim_artif`
#' function for details on the computation time and
#' the exported objects.
#' @return Returns a list of artificial spatiotemporal
#' point patterns generated based on a sample
#' real data.
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
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom tidyr replace_na
#' @importFrom sp SpatialPoints proj4string
#' @importFrom stats predict loess
#' @importFrom lubridate hms
#' @importFrom tibble rownames_to_column
#' @export

psim_real <- function(n_events, ppt, start_date = NULL, poly = NULL,#
                      s_threshold = NULL, step_length = 20,
                      n_origin=50, restriction_feat=NULL,
                      field = NA, p_ratio=20, crsys = NULL){

  idx <- tid <- x <- y <- if_else <- t2 <-
    axis <- . <- locid <- sn <- OriginType <- NULL

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
  #subset xy columns
  spo_xy <- st_properties$origins %>%
    dplyr::select(x, y)#%>%
    #top_n(3)

  #t1 <- Sys.time()

  #estimating computational time
  options(digits.secs = 5)
  tme1 <- Sys.time()
  event_loc_N <- lapply(n, function(n)
    stppSim::walker(n, s_threshold = 100,
                    poly=poly, restriction_feat = restriction_feat,
                    field = field,
                    coords=as.vector(unlist(spo_xy[1,],)),
                    step_length = step_length ,
                    show.plot = FALSE)
  )
  tme2 <- Sys.time()
  #time_elapse <- tme2 - tme1
  time_elapse <- difftime(tme2,tme1,units = "secs")
  time_elapse <- time_elapse + (time_elapse * 0.1)#add 10%
  time_elapse <- round((time_elapse * n_origin)/60, digits=2)
  flush.console()
  cat("#=====")
  cat("The expected computational time for the process is:",paste(time_elapse, " minutes", sep=""),sep=" ")
  cat("=====#")

  #the actual process
  stp_All <- NULL

  if(is.null(s_threshold)){
  for(b in seq_len(nrow(spo_xy))){ #b<-1
    event_loc_N <- lapply(n, function(n)
      stppSim::walker(n, s_threshold = st_properties$s_threshold,
                      poly=poly, restriction_feat = restriction_feat,
                      field = field,
                      coords=as.vector(unlist(spo_xy[b,],)),
                      step_length = step_length ,
                      show.plot = FALSE)
    )

    loc_N <- rbindlist(event_loc_N,
                       use.names=TRUE, fill=TRUE, idcol="tid")

    loc_N <- loc_N %>%
      mutate(locid=b, prob=st_properties$origins$prob[b]) %>%
      mutate(time=format(((tid-1) + as.Date(st_properties$start_date) + hms(time)),
                         "%Y-%m-%d %H:%M:%S"))%>%
      rename(datetime=time)

    stp_All <- stp_All %>%
      bind_rows(loc_N)
  }
  }

  if(!is.null(s_threshold)){
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
      mutate(locid=b, prob=st_properties$origins$prob[b]) %>%
      mutate(time=format(((tid-1) + as.Date(st_properties$start_date) + hms(time)),
                         "%Y-%m-%d %H:%M:%S"))%>%
      rename(datetime=time)

    stp_All <- stp_All %>%
      bind_rows(loc_N)
  }
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

    #sort
    stp_All_ <- stp_All_ %>%
      arrange(locid, tid, sn)

    output[h] <- list(stp_All_)
  }


  #combine and add as details
  #-------------------------------------------
  #add the origins
  output$origins <- st_properties$origins
  output$poly <- st_properties$poly
  output$resist <- restriction_feat
  return(output)

}
