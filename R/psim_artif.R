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
#' a polygon shapefile defining the extent of the landscape.
#' @param netw (An sf or S4 object)
#' The network path of the landscape
#' (e.g. road and/or street). Default: \code{NULL}.
#' If provided each event is snapped to the closest
#' network path/segment.
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
#' @param mfocal the c(x, y) coordinates of a single point,
#' representing a pre-defined `main` focal point (origin)
#' in the area. The default is `NULL` in which a random
#' coordinate is chosen within the `polygon` area.
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
#' @param s_band distance bandwidth that maximizes space and time
#' interaction. It is a vector of two distance values. Default: NULL.
#' @param step_length the maximum step taken
#' by a walker from one point to the next.
#' @param trend specifies the direction of the
#' long-term trend. Options are:
#' `"falling"`, `"stable"`,
#' and `"rising"`. Default value is: `"stable"`.
#' @param slope slope of the long-term trend when
#' an `"rising"` or `"falling"` trend is specified.
#' Options: `"gentle"` or `"steep"`. The default value is
#' set as \code{NULL} for the `stable` trend.
#' @param shortTerm type of short- to medium-term
#' fluctuations (patterns) of the time series.
#' Options are: \code{`"cyclical"` and `"acyclical"`}.
#' Default is: \code{`"cyclical"`}.
#' @param Tperiod time interval (in days) associated with
#' the short term pattern. Default value is \code{90}
#' indicating the first seasonal
#' peak of cyclical short term. For `"acyclical"` short term pattern
#' a single value, e.g. 14, or a list of values,
#' e.g. c(7, 14, 21), can be supplied.
#' @param show.plot (logical) Shows GTP.
#' Default is \code{FALSE}.
#' @param show.data (TRUE or FALSE) To show the output
#' data. Default is \code{FALSE}.
#' @param interactive Whether to run the process in
#' interactive mode. Default is \code{FALSE}. If \code{TRUE},
#' a user is able to preview the spatial and temporal models
#' of the expected distribution of the final simulated
#' events (points).
#' @param ... additional arguments to pass from
#' \code{gtp}, \code{walker} and \code{artif_spo}
#' functions.
#' @usage psim_artif(n_events=1000, start_date = "yyyy-mm-dd",
#' poly, netw = NULL, n_origin, restriction_feat=NULL, field,
#' n_foci, foci_separation, mfocal = NULL, conc_type = "dispersed",
#' p_ratio, s_threshold = 50, s_band = NULL, step_length = 20,
#' trend = "stable", shortTerm = "cyclical", Tperiod=NULL,
#' slope = NULL, interactive = FALSE, show.plot=FALSE, show.data=FALSE, ...)
#' @examples
#' \dontrun{
#'
#' #load boundary and land use of Camden
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' boundary = camden$boundary # get boundary
#' landuse = camden$landuse # get landuse
#'
#' #In this example, we will use a minimal number of
#' #'n_origin' (i.e. `20`) for faster computation:
#'
#' #simulate data
#' simulated_stpp <- psim_artif(n_events=200, start_date = "2021-01-01",
#' poly=boundary, netw = NULL, n_origin=20, restriction_feat = NULL,
#' field = NULL,
#' n_foci=1, foci_separation = 10, mfocal = NULL,
#' conc_type = "dispersed",
#' p_ratio = 20, s_threshold = 50, s_band = NULL, step_length = 20,
#' trend = "stable", shortTerm = "cyclical", Tperiod=NULL,
#' slope = NULL, interactive = FALSE, show.plot=FALSE, show.data=FALSE)
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
#' Both the walkers
#' and the landscape are configured arbitrarily (in accordance
#' with the users knowledge of the domain.
#' This function is computationally intensive. When run,
#' an estimate of the expected computational time
#' is first printed in the console for the user.
#' Argument with the largest impacts on the computational
#' time include `n_origin=50`, and `restriction_feat` when
#' not \code{NULL}. Note: the `n_events`
#' argument has little of no impacts on the
#' computational time, and so it is recommended that
#' that a user inputs a vector of several values
#' to simulate.
#' Lastly, in addition to exporting the simulated
#' point patterns, the
#' function also returns the simulated origins,
#' the boundary and the restriction features
#' (if supplied).
#' @return Returns a list of artificial spatiotemporal
#' point patterns generated from scratch.
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @importFrom dplyr mutate bind_rows select
#' summarise left_join rename arrange
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom graphics points legend
#' @importFrom lubridate hms
#' @importFrom ggplot2 ggplot
#' @importFrom stats lm
#' @importFrom otuSummary matrixConvert
#' @importFrom sf st_nearest_points st_length
#' st_cast
#' @export
#'

psim_artif <- function(n_events=1000, start_date = "yyyy-mm-dd",
                       poly, netw = NULL, n_origin, restriction_feat=NULL, field=NA,
                       n_foci, foci_separation, mfocal = NULL,
                       conc_type = "dispersed", p_ratio,
                       s_threshold = 50, s_band = NULL, step_length = 20,
                       trend = "stable", shortTerm = "cyclical",
                       Tperiod=NULL,
                       slope = NULL, interactive = FALSE, show.plot=FALSE, show.data=FALSE,...){

  #define global variables...
  nrowh <- origins <- locid <- sn <- prob <- z <-
    datetime <- distVal <- ids<- filterField1 <-
    filterField2 <- ID <- ID2 <- NULL

  #first derive the spo object
  spo <- artif_spo(poly, n_origin =  n_origin, restriction_feat = restriction_feat,
                   n_foci=n_foci, foci_separation = foci_separation,
                   mfocal = mfocal, conc_type = conc_type, p_ratio = p_ratio)

  #
  if(shortTerm == "acyclical" & is.null(s_band)) {
    stop(" 's_band' argument cannot be NULL for 'acyclical' short term pattern!")
  }

  #if one value is inputted
  if(length(s_band) != 2){
    stop(" 's_band' needs to be vector of two values!")
  }

  #if one value is inputted
  if(s_band[1] >= s_band[2]){
    stop(" 's_band' value 2 cannot be greater than value 1!")
  }


  #start_date <- as.Date(start_date)

  #check that start_date has value
  if(start_date == "yyyy-mm-dd"){
    stop("Error! 'start_date' argument has to be a real date!")
  }

  #check first peak value
  if(is.null(Tperiod)){
    #Tperiod <- as.Date(start_date) + 90
    Tperiod <- 90
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
    dplyr::select(x, y)


  #testing if crs' are the same
  if(!is.null(netw)){
    crs_poly <- sf::st_crs(poly)$epsg
    crs_netw <- sf::st_crs(netw)$epsg

    if(crs_poly != crs_netw){
      stop("Project of 'poly' and 'netw' shapefiles are not the same!! ")
    }
  }


  #simulate the global temporal pattern
  gtp <- gtp(start_date = start_date, trend, slope=slope,
             shortTerm = shortTerm,
             Tperiod=Tperiod,
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

  #subset xy columns
  spo_xy <- as_tibble(spo$origins) %>%
    dplyr::select(x, y)

  #check the length and
  #and the possible outputs
  if(length(n_events) > 1 & shortTerm == "acyclical"){
    cat(paste0("Note: One event set (i.e. n_events=", n_events[1], ")",
               " will be generated for acyclical short term pattern!!"))
  }


  if(interactive == TRUE){
  #Create spatial and temporal models
  #-----

  cat(paste("#-------------------------------------------#",
              "#-------------------------------------------#",
              "#-------------------------------------------#",sep="\n"))
  cat("                                             ")

  query1 <- readline(prompt = "Preview Spatial and Temporal model? (Y / N):")

  if(query1 %in% c("Y", "y")){

    stm(pt = spo$origins %>%
                    select(x, y, prob), poly=spo$poly, df=gtp$data,
                    crsys = projection(spo$poly), display_output = TRUE)
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
  #     stop("Invalid input! 'N' or 'n' expected! Process terminated!")
  #   }
  #
  # if(query1 %in% c("N","n")){
  #   #do nothing
  # }

  }

  #-----
  #estimating computational time
  options(digits.secs = 5)
  tme1 <- Sys.time()
  event_loc_N <- lapply(n, function(n)
    stppSim::walker(n, s_threshold = s_threshold, #
                    poly=poly, restriction_feat = restriction_feat,
                    field = field,
                    coords=as.vector(unlist(spo_xy[1,],)),
                    step_length = step_length ,
                    show.plot = FALSE)
  )
  tme2 <- Sys.time()
  #time_elapse <- tme2 - tme1
  time_elapse <- difftime(tme2,tme1,units = "secs")
  time_elapse <- round((time_elapse * n_origin)/60, digits=2)
  flush.console()
  time_elapse <- time_elapse + (time_elapse * 0.1)#add 10%
  cat("#=====")
  cat("*--------- Expected time of execution: ",paste(time_elapse, " minutes ---------*", sep=""),sep=" ")
  cat("=====#")

  #preview
  #-----------------
  loc_N <- rbindlist(event_loc_N,
                   use.names=TRUE, fill=TRUE, idcol="tid")
  loc_N <- loc_N%>%dplyr::filter(tid==1)
  #dev.new()
  plot(loc_N$x, loc_N$y)
  #-----------------

  #the actual process
  stp_All <- NULL

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
      mutate(locid=b, prob=spo$origins$prob[b]) %>%
      mutate(time=format(((tid-1) + as.Date(start_date) + hms(time)),
                         "%Y-%m-%d %H:%M:%S"))%>%
      dplyr::rename(datetime=time)

    stp_All <- stp_All %>%
      bind_rows(loc_N)

    }


  #saveRDS(stp_All, file="C:/Users/monsu/Documents/GitHub/sppSim backup/backup 27012023/stppSimbackupArtif_acyclical_60.rds")

#-------------------------------------

  #to smooth the short and medium pattern
  #when noncyclic

    if(shortTerm == "acyclical"){

      datxy <- stp_All

      #divide the data and keep backup
      s_id <- sample(1:nrow(datxy), round(nrow(datxy)*0.75, digits = 0), replace=FALSE)
      div_75 <- datxy[s_id, ]

      div_25 <- datxy[!1:nrow(datxy)%in%s_id,]

      datxy_plot <- div_75 %>%
        #datxy_plot <- stp_All_sub %>%
        dplyr::mutate(t = as.Date(substr(datetime, 1,10)))%>%
        group_by(t) %>%
        summarise(n = n()) %>%
        mutate(time = as.numeric(difftime(t, as.Date("2020-12-31"), units="days")))%>%
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
      #library(ggplot2)

      # ggplot(loessData1, aes(x, y)) +
      #   geom_point(dat = datxy_plot, aes(time, n), alpha = 0.2, col = "red") +
      #   geom_line(col = "blue") +
      #   facet_wrap(~method) +
      #   ggtitle("Interpolation and smoothing functions in R") +
      #   theme_bw(16)

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

      filtered_stp_All
  #}

  #repeat filter for spatial and temporal thresholds

  fN_final_dt_convert <- NULL

  ori_sn <- unique(filtered_stp_All$locid)[order(unique(filtered_stp_All$locid))]

  for(or in seq_len(length(ori_sn))){ #or=1

    ##plot(filtered_stp_All$x, filtered_stp_All$y)
    ##plot(sub_Dat$x, sub_Dat$y)

    sub_Dat <- filtered_stp_All %>%
      dplyr::filter(locid == ori_sn[or])

    sample_sub_Dat <- sub_Dat[sample(1:nrow(sub_Dat), 5000, replace = FALSE),]

    # somesample <- sample_sub_Dat[sample(1:nrow(sample_sub_Dat), 1052, replace = FALSE),]
    # tme <-as.numeric(as.Date(somesample$datetime))#[1:10]
    # ##xy <- data.frame(x=sample_sub_Dat$x, y=sample_sub_Dat$y)#[1:10,]
    # dt = dist(tme)
    # dim(dt)
    # hist(dt, 365)

    tme <-as.numeric(as.Date(sample_sub_Dat$datetime))#[1:10]
    dt = dist(tme)

    #for a specified time threshold
    dt_convert <- matrixConvert(dt, colname = c("cname", "rname", "distVal"))
    #head(dt_convert)

    #given theshold
    #t_threshold <- c(7,  14)
    #t_threshold <- c(0:365)

    #maximize the occurence of this threshold
    dt_conver_Wthres <- dt_convert %>%
      dplyr::filter(distVal %in% Tperiod)

    dt_conver_Wthres_Comb <- data.frame(ids = c(dt_conver_Wthres$cname,  dt_conver_Wthres$rname),
                                        distVal=rep(dt_conver_Wthres$distVal, 2))

    dt_conver_Wthres_Comb_ <- dt_conver_Wthres_Comb %>%
      dplyr::group_by(ids) %>%
      dplyr::summarise(n=n())%>%
      dplyr::arrange(desc(n))%>%
      dplyr::top_frac(0.1) #top 10

    final_dt_convert <- sample_sub_Dat[dt_conver_Wthres_Comb_$ids,]


    #----------------------------------------------------------------------------distance part

    #apply distance threshold
    xy <- data.frame(x=final_dt_convert$x, y=final_dt_convert$y)#[1:10,]

    ds <- dist(xy)
    #for a specified time threshold
    ds_convert <- matrixConvert(ds, colname = c("cname", "rname", "distVal"))
    #head(ds_convert)

    #s_threshold <- c(100,200)

    #maximize the occurence of this threshold
    ds_conver_Wthres <- data.frame(ds_convert) %>%
      dplyr::mutate(filterField1 = if_else(distVal >= min(s_band), paste("yes"), paste("no"))) %>%
      dplyr::mutate(filterField2 = if_else(distVal <= max(s_band), paste("yes"), paste("no"))) %>%
      dplyr::filter(filterField1 == "yes" & filterField2 == "yes")

    ds_conver_Wthres_Comb <- data.frame(ids = c(ds_conver_Wthres$cname,  ds_conver_Wthres$rname),
                                        distVal=rep(ds_conver_Wthres$distVal, 2))

    ds_conver_Wthres_Comb_ <- ds_conver_Wthres_Comb %>%
      dplyr::group_by(ids) %>%
      dplyr::summarise(n=n())%>%
      dplyr::arrange(desc(n))%>%
      dplyr::top_frac(0.1) #top 10

    final_ds_convert <- final_dt_convert[ds_conver_Wthres_Comb_$ids,]

    #----------------------------------------------------------------------------------------



    fN_final_dt_convert <- rbind(fN_final_dt_convert,
                                 final_ds_convert)

    flush.console()
    print(or)

  }

  stp_All <- fN_final_dt_convert

    }
#--------------------------------------#stp_All

  if(length(n_events) > 1 & shortTerm == "acyclical"){
    n_events <- n_events[1]
  }

  #Also if the final list is very small compare
  #to the list needed
  if(nrow(stp_All) < round(n_events[1]*1.5, digits = 0)){
    cat(paste0("*------------| A total of ",  round(nrow(stp_All)*0.75, digits = 0),
              " events are generated! |--------------*"))

    n_events <- round(nrow(stp_All)*0.75, digits = 0)
  }


  #generate all the results
  for(h in seq_len(length(n_events))){ #h<-1

    #add idx
    stp_All_ <- stp_All %>%
      rownames_to_column('ID') #%>% #add row as column

    #sample to derive required number
    samp_idx <- as.numeric(sample(stp_All_$ID, size = n_events[h],
                                replace = FALSE, prob = stp_All_$prob)) #%>

    stp_All_ <- stp_All_[samp_idx, ]

    #sort
    stp_All_ <- stp_All_ %>%
      arrange(locid, tid, sn) %>%
      rownames_to_column('ID2') %>%
      dplyr::select(-c(ID))%>%
      dplyr::rename(ID = ID2)

    output[h] <- list(stp_All_)
  }

  #snap function
  st_snap_points = function(x, y, max_dist = 1000) {

    if (inherits(x, "sf")) n = nrow(x)
    if (inherits(x, "sfc")) n = length(x)

    out = do.call(c,
                  lapply(seq(n), function(i) {
                    nrst = st_nearest_points(st_geometry(x)[i], y)
                    nrst_len = st_length(nrst)
                    nrst_mn = which.min(nrst_len)
                    if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                    return(st_cast(nrst[nrst_mn], "POINT")[2])
                  })
    )
    return(out)
  }

  #if network path is provided
  if(!is.null(netw)){

    #convert point to geometry type
    output_pt <- data.frame(output) %>%
      st_as_sf(coords = c("x", "y"), crs = crs_netw, remove =F)#%>%
    #tibble::rownames_to_column("id")

    system.time(snappedData <- st_snap_points(output_pt, netw))
    #plot(snappedData)

    output[[1]]$x <- st_coordinates(snappedData)[,1]
    output[[1]]$y <- st_coordinates(snappedData)[,2]
  }


  #add the origins
  output$origins <- spo$origins
  output$mfocal <- spo$mfocal
  output$poly <- spo$poly
  output$resist <- restriction_feat

  #insert network path
  if(!is.null(netw)){
    output$netw <- netw
  }

  return(output)
}
