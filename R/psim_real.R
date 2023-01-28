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
#' @param interactive Whether to run the process in
#' interactive mode. Default is \code{FALSE}. If \code{TRUE},
#' a user is able to preview the spatial and temporal models
#' of the expected distribution of the final simulated
#' events (points).
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
#' p_ratio=20, interactive = FALSE, crsys = NULL)
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
#' p_ratio=20, interactive = FALSE, crsys = "EPSG:27700")
#'
#' #If `n_events` is a vector of values,
#' #retrieve the simulated data for the
#' #corresponding vector element by using
#' #`simulated_stpp[[enter-element-index-here]]`, e.g.,
#' #to retrieve the first dataframe, use
#' #simulated_stpp[[1]].
#'
#' #The above example simulates point patterns on
#' #an unrestricted landscape. If
#' #`restriction_feat = landuse` and
#' #`field = "restrVal"`, then the simulation
#' #is run with the landuse features as restrictions
#' #on the landscape.
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
#' desc filter count pull
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom tidyr replace_na
#' @importFrom sp SpatialPoints proj4string
#' @importFrom stats predict loess
#' @importFrom lubridate hms
#' @importFrom tibble rownames_to_column
#' @importFrom Rcpp cppFunction
#' @importFrom graphics hist
#' @export

psim_real <- function(n_events, ppt, start_date = NULL, poly = NULL,#
                      s_threshold = NULL, step_length = 20,
                      n_origin=50, restriction_feat=NULL,
                      field = NA, p_ratio=20, interactive = FALSE,
                      crsys = NULL){

  idx <- tid <- x <- y <- if_else <- t2 <-
    axis <- . <- locid <- sn <- OriginType <-
    prob <- NULL

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
    dplyr::select(x, y)

  #get the global temporal pattern
  n = st_properties$gtp#[1:4]
  #
  #subset xy columns
  spo_xy <- st_properties$origins %>%
    dplyr::select(x, y)#%>%
    #top_n(3)

  #t1 <- Sys.time()
  if(interactive == TRUE){
    #Create spatial and temporal models
    #-----

    cat(paste("#-------------------------------------------#",
              "#-------------------------------------------#",
              "#-------------------------------------------#",sep="\n"))
    cat("                                             ")

    query1 <- readline(prompt = "Preview Spatial and Temporal model? (Y / N):")

    if(query1 %in% c("Y", "y")){

      stm(pt = st_properties$origins %>%
            select(x, y, prob), poly=st_properties$poly, df=st_properties$gtp,
          crsys = projection(st_properties$poly), display_output = TRUE)
      #flush.console()
      cat(paste("#-----------------#",
                "#-----------------#",
                "#-----------------#",sep="\n"))
      cat("                   ")
      query2 <- readline(prompt = "Continue? (Y / N):")

      if(!query2 %in% c("N", "n", "Y", "y")){
        stop("Invalid input! 'Y' or 'N', expected! Process terminated!")
      }

      if(query2 %in% c("N", "n")){
        stop("Process terminated!")
      }

      if(query2 %in% c("Y", "y")){
        #continue processing
      }
    }

    # if(!query1 %in% c("N","n")){
    #   stop("Invalid input! 'N' or 'n' expected! Process terminated!")
    # }
    #
    # if(query1 %in% c("N","n")){
    #   #do nothing
    # }

  }



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
      dplyr::rename(datetime=time)

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
      dplyr::rename(datetime=time)

    stp_All <- stp_All %>%
      bind_rows(loc_N)
  }
  }

  #-------------------------
  #C++ function to do sampling based on probability field
  cppFunction('IntegerVector rcpp_sample_prob(const IntegerVector& ind_sample, int N, int n) {

  LogicalVector is_chosen(N);
  IntegerVector ind_chosen(n);

  int i, k, ind;

  for (k = 0, i = 0; i < n; i++) {
    do {
      ind = ind_sample[k++];
    } while (is_chosen[ind-1]);
    is_chosen[ind-1] = true;
    ind_chosen[i] = ind;
  }

  return ind_chosen;
}')

  sample_fast <- function(n, prb) {
    N <- length(prb)
    sample_more <- sample.int(N, size = 2 * n, prob = prb, replace = TRUE)
    rcpp_sample_prob(sample_more, N, n)
  }
  #-------------------------

  #-----------------------------------------------
  #get repeat pattern in real (across the whole area)
  #-----------------------------------------------
  #n <- 365 #daily temporal resolution (accuracy)

  ppt_diff <- ppt %>%
    dplyr::select(date) %>%
    dplyr::mutate(tme = as.numeric(as.Date(date)))%>%
    dplyr::mutate(tmeDiff = tme - min(tme)) #from origin
#head(ppt_diff)
  # tme <-as.numeric(as.Date(ppt$date))#[1:10] #check patterns first
  # t_origin <- min(tme)
  # dt_origin = tme - t_origin
  #hist(dt_origin, n)

  probList <- hist(ppt_diff %>% dplyr::pull(tmeDiff), 365)
  probList <- c(0, probList$density)
  probList <- data.frame(date = seq.Date(from = min(as.Date(ppt$date)), to = min(as.Date(ppt$date))+364, by = 'days'),
                         probVal = probList/sum(probList))

  fnDateList <- probList %>%
    dplyr::mutate(tme = as.numeric(as.Date(date)))%>%
    dplyr::mutate(tme=paste0("D",as.character(tme)))#%>%

  #head(probList)
  #join the dataList
  # fnDateList <- ppt_diff %>%
  #   dplyr::left_join(probList) %>%
  #   #dplyr::select(tmeDiff, probVal)%>%
  #   #dplyr::select(-c(date, tmeDiff))%>%
  #   dplyr::mutate(tme=paste0("D",as.character(tme)))%>%
  #   dplyr::arrange(tme)

  #get the proportions
  # head(stp_All)
  # stp_All %>%
  #   dplyr::group_by(locid) %>%
  #   count()

  #plot(probList$valDist,probList$probVal, type="l")
  #probList

  #n_events <- c(2000, 3000)
  #reformat/generate all the results
  for(h in seq_len(length(n_events))){

    #add idx
    stp_All_ <- stp_All %>%
      rownames_to_column('ID') #%>% #add row as column

    #head(stp_All_)
    nrow(stp_All_)
    #----------------------------
    #apply repeat filtering
    # tme <-as.numeric(as.Date(stp_All_subset$datetime))#[1:10] #check patterns first
    # dt = dist(tme)
    # dt_vector <- as.vector(dt)
    # hist(dt_vector, 365)

    ##orList <- unique(stp_All_$locid)

    ##for(or in seq_len(length(orList))){ #or<-1

      stp_All_subset <- stp_All_ %>%
        ##dplyr::filter(locid == orList[or]) %>%
        data.frame() %>%
        #dplyr::select(datetime)%>%
        dplyr::mutate(tme= paste0("D", as.character(as.numeric(as.Date(datetime))))) %>%
        dplyr::left_join(fnDateList)

        #for sampling, define probability val and n
        prb <- stp_All_subset$probVal
        n <- round(nrow(stp_All_subset)/2, digits = 0) #select half of the data

        #to call sampling function c++
        sample_fast <- function(n, prb) {
          N <- length(prb)
          sample_more <- sample.int(N, size = 2 * n, prob = prb, replace = TRUE)
          rcpp_sample_prob(sample_more, N, n)
        }

        system.time(ind <- sample_fast(n, prb))

        subsetFn <- stp_All_[ind, ]

        #nrow(subsetFn)
        # tme <-as.numeric(as.Date(subsetFn$datetime))#[1:10] #check patterns first
        # dt <- dist(tme)
        #hist(dt, 365)
    #}


    #----------------------------
    #sample to derive required number
    samp_idx <- as.numeric(sample(subsetFn$ID, size = n_events[h],
                                  replace = FALSE, prob = subsetFn$prob)) #%>

    subsetFn <- subsetFn[which(subsetFn$ID %in% samp_idx), ]

    #sort
    subsetFn <- subsetFn %>%
      arrange(locid, tid, sn)

    output[h] <- list(subsetFn)
  }


  #combine and add as details
  #-------------------------------------------
  #add the origins
  output$origins <- st_properties$origins
  output$poly <- st_properties$poly
  output$resist <- restriction_feat

  return(output)

}
