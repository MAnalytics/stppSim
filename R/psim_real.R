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
#' (in the format: `yyyy-mm-dd')
#' @param start_date the start date of the temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The temporal pattern will normally cover
#' 1-year period.
#' @param poly (An sf or S4 object)
#' a polygon shapefile defining the extent of the landscape
#' @param netw (An sf or S4 object)
#' The network path of the landscape
#' (e.g. road and/or street). Default: \code{NULL}.
#' If provided each event is snapped to the closest
#' network path/segment.
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
#' @param s_range A value (in metres), not less than 150,
#' specifying the maximum range of spatial
#' interaction across the space. For example, for 150m,
#' the intervals of spatial interactions are created as
#' \code{(0, 50]}, \code{(50 - 100]}, and \code{(100-150]},
#' representing the "small", "medium", and "large",
#' spatial interaction ranges, respectively. If
#' `s_range` is set as `NULL`, simulation
#' focusses only on generating point pattern with
#' similar spatiotemporal patterns as the sample
#' dataset.
#' @param s_interaction (string) indicating the
#' type of spatial interaction to detect.
#' Default: \code{"medium"} (See parameter \code{'s_range'})
#' @param tolerance Pvalue to use for the extraction of
#' space-time interaction in the sample data. Default
#' value: \code{0.05}.
#' @param crsys (string) the EPSG code of the projection
#' system of the `ppt` coordinates. This is only used if
#' `poly` argument is \code{NULL}.
#' See "http://spatialreference.org/" for the list of
#' EPSG codes for different regions of the world.
#' As an example, the EPSG code for the British National Grid
#' projection system is: \code{"EPSG:27700"}.
#' @usage psim_real(n_events, ppt, start_date = NULL, poly = NULL,
#' netw = NULL, s_threshold = NULL, step_length = 20, n_origin=50,
#' restriction_feat=NULL, field=NA,
#' p_ratio=20, interactive = FALSE, s_range = 150,
#' s_interaction = "medium", tolerance = 0.07,
#' crsys = NULL)
#' @examples
#' \dontrun{
#' data(camden_crimes)
#' #subset 'theft' crime
#' theft <- camden_crimes[which(camden_crimes$type == "Theft"),]
#' #specify the proportion of full data to use
#' sample_size <- 0.3
#' set.seed(1000)
#' dat_sample <- theft[sample(1:nrow(theft),
#' round((sample_size * nrow(theft)), digits=0),
#' replace=FALSE),1:3]
#' #plot(dat_sample$x, dat_sample$y) #preview
#'
#' #load boundary and land use of Camden
#' #load(file = system.file("extdata", "camden.rda",
#' #package="stppSim"))
#' #landuse = camden$landuse # get landuse
#' landuse <- stppSim:::landuse
#' #simulate data
#' simulated_stpp <- psim_real(n_events=2000, ppt=dat_sample,
#' start_date = NULL, poly = NULL, netw = NULL, s_threshold = NULL,
#' step_length = 20, n_origin=20,
#' restriction_feat = NULL, field=NULL,
#' p_ratio=20, interactive = FALSE, s_range = 150,
#' s_interaction = "medium", tolerance = 0.07,
#' crsys = "EPSG:27700")
#' #If `n_events` is a vector of values,
#' #retrieve the simulated data for the
#' #corresponding vector element by using
#' #`simulated_stpp[[enter-element-index-here]]`, e.g.,
#' #to retrieve the first dataframe, use
#' #simulated_stpp[[1]].
#'
#' #The above example simulates point patterns on
#' #an unrestricted landscape. If \code{restriction_feat = landuse} and \code{field = "restrVal"},
#' then the simulation
#' #is run with the landuse features as restrictions
#' #on the landscape.
#' }
#' @details
#' The spatial and temporal patterns and
#' interactions detected in sample datasets
#' are extrapolated to synthetise larger
#' data size. Details of the spatiotemporal
#' interactions detected in the sample
#' dataset are provided. If the street network
#' of the area is provided, each point is
#' snapped to its nearest street segment.
#' @return A list of artificial spatiotemporal
#' point patterns and interaction generated based on a sample
#' (real) data.
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
#' @importFrom tibble rownames_to_column tibble
#' @importFrom graphics hist
#' @importFrom sf st_nearest_points st_length
#' st_cast
#' @export

psim_real <- function(n_events, ppt, start_date = NULL, poly = NULL,#
                      netw = NULL, s_threshold = NULL, step_length = 20,
                      n_origin=50, restriction_feat=NULL,
                      field = NA, p_ratio=20, interactive = FALSE,
                      s_range =  150,
                      s_interaction = "medium",
                      tolerance = 0.07,
                      crsys = NULL){

  idx <- tid <- x <- y <- if_else <- t2 <-
    axis <- . <- locid <- sn <- OriginType <-
    prob <- cname <- rname <- locID_sub <-
    distVal1 <- distVal2 <- distVal <-
    tme <- tmeDiff<- datetime <-
    str_length <- NULL

  #check s_interaction
  if(!s_interaction %in% c("small",
                           "medium",
                           "large") & !is.null(s_range)){
    stop("Argument 's_interaction' is invalid!!")
  }

  output <- list()

  #ppt <- burg_df_samp
  st_properties <- stp_learner(ppt=ppt, start_date = start_date,
                               poly = poly, n_origin=n_origin,
                               p_ratio = p_ratio,
                               s_range =  s_range, tolerance = tolerance,
                               crsys = crsys)

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

  #testing if crs' are the same
  if(!is.null(netw)){
    crs_poly <- sf::st_crs(poly)$epsg
    crs_netw <- sf::st_crs(netw)$epsg

    if(crs_poly != crs_netw){
      stop("Project of 'poly' and 'netw' shapefiles are not the same!! ")
    }
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


  #note resist feature should be .shp
  #estimating computational time
  options(digits.secs = 5)
  tme1 <- Sys.time()
  event_loc_N <- lapply(n, function(n)
    stppSim::walker(n, s_threshold = st_properties$s_threshold,
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
  cat("*--------- Expected time of execution: ",paste(time_elapse, " minutes ---------*", sep=""),sep=" ")
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

  #
  #saveRDS(stp_All, file = "realsave_70_origins.rds")

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
      #
      # saveRDS(stp_All,
      # file="stppSimbackupReal_040.rds")

    }

  }

  #---------------------------
  #to adjust the baseline of time series
  datxy <- stp_All

  #divide the data and keep backup
  s_id <- sample(1:nrow(datxy), round(nrow(datxy)*0.6, digits = 0), replace=FALSE)
  div_75 <- datxy[s_id, ]

  div_25 <- datxy[!1:nrow(datxy)%in%s_id,]

  datxy_plot <- div_75 %>%
    dplyr::mutate(t = as.Date(substr(datetime, 1,10)))%>%
    dplyr::group_by(t) %>%
    summarise(n = dplyr::n()) %>%
    mutate(time = as.numeric(difftime(t, as.Date(st_properties$start_date)-1, units="days")))%>%
    as.data.frame()

  time <- data.frame(time=1:365)

  datxy_plot <- time %>%
    left_join(datxy_plot) %>%
    dplyr::mutate(n = replace_na(n, 0))

  #unique(dat_sample_p$time)

  ##plot(datxy_plot$time, datxy_plot$n, 'l')
  ##head(datxy)

  #fit and plot
  loessData1 <- data.frame(
    x = 1:nrow(datxy_plot),
    y = predict(lm(n~time, datxy_plot)),
    method = "loess()"
  )


  loessData1 <- round(loessData1$y, digits = 0)

  #randomly remove point for each day data
  filtered_stp_All <- NULL

  for(rmv in seq_len(length(loessData1))){ #rmv = 1

    #data to either reduce or increase
    stp_All_on_day <- div_75 %>%
      dplyr::filter(tid == rmv)

    if(datxy_plot$n[rmv] >= loessData1[rmv]){
      orig_pt <- sample(1:datxy_plot$n[rmv], loessData1[rmv], replace = FALSE)
      todaysData <- stp_All_on_day[orig_pt, ]
    }

    if(datxy_plot$n[rmv] < loessData1[rmv]){
      #first borrow the remainder
      rem_D <- div_25[sample(1:nrow(div_25), (loessData1[rmv] - datxy_plot$n[rmv]), replace = F),]
      todaysData <- rbind(stp_All_on_day, rem_D)
    }

    filtered_stp_All <- rbind(filtered_stp_All, todaysData)

  }

  if(!is.null(st_properties$st_sign)){

    if(s_interaction == "small"){
      st <- 1
    }
    if(s_interaction == "medium"){
      st <- 2
    }
    if(s_interaction == "large"){
      st <- 3
    }

  }

  #-----------------------------------------------
  #integrate the spatiotemporal sign.
  #-----------------------------------------------

  #temporal bandwidths
  t_list <- st_properties$st_sign
  #t_list <- as.data.frame(do.call(rbind, t_list))[, 2:3]

  event_Collate <- NULL

  fN_final_dt_convert <- NULL

  #loop by origin
  ori_sn <- unique(filtered_stp_All$locid)[order(unique(filtered_stp_All$locid))]

  #if st_sign is not detected or set as null

  if(!is.null(st_properties$st_sign)){

    st_signature_filtered <- Filter(Negate(is.null), st_properties$st_sign)

    #prepare the st signature table
    signTable <- NULL

    init_n <- 0

    if(length(st_signature_filtered[[st]])>=1 &
       !"NA" %in% st_signature_filtered[[st]]){

      #st_collate <- NULL

      for(or in seq_len(length(ori_sn))){ #or=1

        sub_Dat <- filtered_stp_All %>%
          rownames_to_column('locID_sub') %>%
          dplyr::filter(locid == ori_sn[or])

        sample_sub_Dat <- sub_Dat[sample(1:nrow(sub_Dat),
                                         round(nrow(sub_Dat)*0.5, digits = 0),
                                         replace = FALSE),]

        tme <-as.numeric(as.Date(sample_sub_Dat$datetime))#[1:10]
        dt = dist(tme)

        #for a specified time threshold
        dt_convert <- matrixConvert(dt, colname = c("cname", "rname", "distVal"))

        #get t threshold
        t_st <- st_signature_filtered[[st]]
        t_st <- st_properties$t_bands[t_st]
        #t_st <- as.vector(unlist(t_list[t_st, ]))

        #collapse the temporal
        t_st_List <- NULL
        for(q in 1:length(t_st)){#q<-1
          tsm <- substring(t_st[q], 2, str_length(t_st[q]))
          tsm <- substring(tsm, 1, str_length(tsm)-1)
          tsm <- as.numeric(strsplit(tsm, split= ',', fixed=TRUE)[[1]])
          t_st_List <- c(t_st_List, tsm[1]:(tsm[2]-1))
        }

        #maximize the occurence of t threshold
        dt_conver_Wthres <- dt_convert %>%
          tibble()%>%
          dplyr::filter(distVal %in%  t_st_List) %>% #[1]
          dplyr::rename(distVal1 = distVal)

        #apply distance threshold
        xy <- data.frame(x=sample_sub_Dat$x, y=sample_sub_Dat$y)#[1:10,]

        ds <- dist(xy)

        ds_convert <- matrixConvert(ds, colname = c("cname", "rname", "distVal"))

        #get names
        nm <- names(st_signature_filtered)[st]
        nm <- substring(nm, 2, str_length(nm))
        nm <- substring(nm, 1, str_length(nm)-1)
        nm <- as.numeric(strsplit(nm, split= ',', fixed=TRUE)[[1]])

        #maximize the occurence of s threshold
        ds_convert2 <- ds_convert %>%
          dplyr::rename(distVal2 = distVal) %>%
          dplyr::mutate(distVal2 = round(distVal2, digits = 0)) %>%
          dplyr::filter(distVal2 %in% c(nm[1]:nm[2]))

        f_count <- ds_convert2 %>%
          dplyr::left_join(dt_conver_Wthres) %>%
          dplyr::filter(!is.na(distVal1))

        f_count_col <- f_count %>%
          dplyr::group_by(cname)%>%
          dplyr::count()%>%
          dplyr::arrange(desc(n))%>%
          data.frame()%>%
          dplyr::top_frac(0.05)

        f_count_row <- f_count %>%
          dplyr::group_by(rname)%>%
          dplyr::count()%>%
          dplyr::arrange(desc(n))%>%
          data.frame()%>%
          dplyr::top_frac(0.05)


        dt_conver_Wthres_Comb <- data.frame(ids = c(f_count_row$rname, f_count_col$cname))
        ids <- unique(dt_conver_Wthres_Comb$ids)
        sample_sub_DatNew <- sample_sub_Dat[as.numeric(ids),]

        #-----------------------------------------------------------
        if(length(ids) != 0){
          event_Collate <- rbind(event_Collate,  sample_sub_DatNew)
        }
        #----------------event_Collate--------------------------...#

        # flush.console()
        # print(st)
        # print(nrow(event_Collate))

        event_Collate <- event_Collate %>%
          dplyr::filter(!duplicated(locID_sub))

        init_n <- nrow(event_Collate)

        flush.console()
        print(or)
        print(init_n)

      }

    }

  }

  if("NA" %in% st_signature_filtered[[st]] | is.null(st_properties$st_sign)){

    for(or in seq_len(length(ori_sn))){ #or=1

      sub_Dat <- filtered_stp_All %>%
        dplyr::filter(locid == ori_sn[or])

      sample_sub_Dat <- sub_Dat[sample(1:nrow(sub_Dat),
                                       round(nrow(sub_Dat)*0.5, digits = 0),
                                       replace = FALSE),]

      #marking this sample #just added
      sample_sub_Dat <- sample_sub_Dat %>%
        rownames_to_column('locID_sub') #

      event_Collate <- rbind(event_Collate, sample_sub_Dat)

      flush.console()
      print(or)

    }

  }
  #------------------------------------------.......#
  stp_All_bk <- event_Collate
  #------------------------------------------.......#

  for(h in seq_len(length(n_events))){ #h<-1

    #add idx
    stp_All_ <- stp_All_bk %>%
      rownames_to_column('ID') #%>% #add row as column

    #head(stp_All_)
    nrow(stp_All_)
    #----------------------------
    #apply repeat filtering
    stp_All_subset <- stp_All_ %>%
      data.frame() %>%
      dplyr::mutate(tme= paste0("D", as.character(as.numeric(as.Date(datetime))))) ##%>%

    #for sampling, define probability val and n
    prb <- stp_All_subset$probVal
    n <- round(nrow(stp_All_subset)/2, digits = 0) #select half of the data

    samp_idx <- as.numeric(sample(stp_All_subset$ID, size = n_events[h],
                                  replace = FALSE, prob = stp_All_subset$prob)) #%>


    subsetFn <- stp_All_[samp_idx, ]


    output[h] <- list(subsetFn)


    #saveRDS(output[[1]],
    #file="real_simulated_for_Detroid_100.rds")

  }


  #snap function
  #if network path is provided
  if(!is.null(netw)){ #netw <- netw_

    flush.console()
    print("***------Generating network data, processing....:")

    for(g in seq_len(length(n_events))){ #g<-1

      #convert point to geometry type
      output_pt <- data.frame(output[g]) %>%
        st_as_sf(coords = c("x", "y"), crs = crs_netw, remove =F)#%>%

      snappedData <- snap_points_to_lines(points=output_pt,
                                          lines=netw,
                                          verbose = FALSE)

      output[[g]]$x <- st_coordinates(snappedData)[,1]
      output[[g]]$y <- st_coordinates(snappedData)[,2]
    }
  }

  #combine and add as details
  #-------------------------------------------
  #add the origins
  output$origins <- st_properties$origins
  output$poly <- st_properties$poly
  output$resist <- restriction_feat
  output$st_interaction_detected <- t_list
  output$st_interaction_simulated <- t_list[st]

  #insert network path
  if(!is.null(netw)){
    output$netw <- netw
  }

  return(output)

}
