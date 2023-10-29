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
#' The 'gtp' will normally cover a 1-year period.
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
#' @param step_length the maximum step taken
#' by a walker from one point to the next.
#' @param trend specifies the direction of the
#' long-term trend. Options are:
#' `"falling"`, `"stable"`,
#' and `"rising"`. Default value is: `"stable"`.
#' @param shortTerm type of short- to medium-term
#' fluctuations (patterns) of the time series.
#' Options are: \code{`"cyclical"` and `"acyclical"`}.
#' Default is: \code{`"cyclical"`}.
#' @param fPeak first seasonal
#' peak of cyclical short term. Default value is \code{90}.
#' Only used for `"cyclical"` short term pattern.
#' @param s_band distance bandwidth within which
#' the event re-occurences are maximized (i.e.,
#' interactions are maximum). Specified as a vector of
#' two distance values. Default: \code{c(0, 200)}.
#' @param t_band temporal bandwidth within which
#' event re-occurences are maximized (i.e., interactions
#' are maximum). Specified as a vector of values (in days)
#' \code{c(1, 5, 7, 14)}.
#' @param slope slope of the long-term trend when
#' an `"rising"` or `"falling"` trend is specified.
#' Options: `"gentle"` or `"steep"`. The default value is
#' set as \code{NULL} for the `stable` trend.
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
#' @usage psim_artif(n_events=1000, start_date = "2021-01-01",
#' poly, netw = NULL, n_origin, restriction_feat=NULL, field,
#' n_foci, foci_separation, mfocal = NULL, conc_type = "dispersed",
#' p_ratio=20, s_threshold = 50, step_length = 20,
#' trend = "stable", shortTerm = "cyclical", fPeak=90,
#' s_band = c(0, 200),
#' t_band = c(1, 5, 10),
#' slope = NULL, interactive = FALSE, show.plot=FALSE, show.data=FALSE, ...)
#' @examples
#' \dontrun{
#'
#' #load boundary and land use of Camden
#' #load(file = system.file("extdata", "camden.rda",
#' #package="stppSim"))
#' #boundary = camden$boundary # get boundary
#' #landuse = camden$landuse # get landuse
#' boundary <- stppSim:::boundary
#' landuse <- stppSim:::landuse
#' #In this example, we will use a minimal number of
#' #'n_origin' (i.e. `20`) for faster computation:
#'
#' #simulate data
#' simulated_stpp <- psim_artif(n_events=200, start_date = "2021-01-01",
#' poly=boundary, netw = NULL, n_origin=20, restriction_feat = NULL,
#' field = NULL,
#' n_foci=1, foci_separation = 10, mfocal = NULL,
#' conc_type = "dispersed",
#' p_ratio = 20, s_threshold = 50,
#' step_length = 20,
#' trend = "stable", shortTerm = "cyclical",
#' fPeak=90, s_band = c(0, 200),
#' t_band = c(1, 5, 10),
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
#' Simulate artificial spatiotemporal patterns
#' and interactions based user specifications.
#' @return Returns a list of artificial spatiotemporal
#' point patterns based on user-defined parameters.
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

psim_artif <- function(n_events=1000, start_date = "2021-01-01",
                       poly, netw = NULL, n_origin, restriction_feat=NULL, field=NA,
                       n_foci, foci_separation, mfocal = NULL,
                       conc_type = "dispersed", p_ratio=20,
                       s_threshold = 50,
                       step_length = 20,
                       trend = "stable", shortTerm = "cyclical",
                       fPeak=90, s_band = c(0, 200), t_band = c(1, 5, 10),
                       slope = NULL, interactive = FALSE, show.plot=FALSE, show.data=FALSE,...){

  #define global variables...
  nrowh <- origins <- locid <- sn <- prob <- z <-
    datetime <- distVal <- ids<- filterField1 <-
    filterField2 <- ID <- ID2 <- distVal1 <-
    distVal2 <- rname <- cname <- st <- locID_sub <- NULL

  #first derive the spo object
  spo <- artif_spo(poly, n_origin =  n_origin, restriction_feat = restriction_feat,
                   n_foci=n_foci, foci_separation = foci_separation,
                   mfocal = mfocal, conc_type = conc_type, p_ratio = p_ratio)

  #spo
  #
  if(shortTerm == "acyclical" & is.null(s_band)) {
    stop(" 's_band' argument cannot be NULL for 'acyclical' short term pattern!")
  }

  if(shortTerm == "acyclical" & is.null(t_band)) {
    stop(" 't_band' argument cannot be NULL for 'acyclical' short term pattern!")
  }

  #if one value is inputted
  if(length(s_band) != 2){
    stop(" 's_band' needs to be vector of two values!")
  }

  if(shortTerm == "acyclical"){
    #if one value is inputted
    if(s_band[1] >= s_band[2]){
      stop(" 's_band' value 2 cannot be greater than value 1!")
    }

    #check whether t_band is integer
    check.integer <- function(N){
      !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
    }


    if(!check.integer(t_band)){
      stop(" 't_band' value must be an integer!")
    }

    # if(t_band >= 90){
    #   stop(" 't_band' value may be too large! Input a smaller value!")
    # }

  }

  if(shortTerm == "cyclical"){
    check.integer <- function(N){
      !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
    }

    if(!check.integer(fPeak)){
      stop(" 'fPeak' value must be an integer!")
    }

    if(fPeak > 90){
      stop(" 'fPeak' value must be less than or equal to 90!")
    }

  }

  #start_date <- as.Date(start_date)

  #check that start_date has value
  if(start_date == "yyyy-mm-dd"){
    stop("Error! 'start_date' argument has to be a real date!")
  }

  #check first peak value
  if(is.null(fPeak)){
    #fPeak <- as.Date(start_date) + 90
    fPeak <- 90
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
  gtp <- gtp(start_date = start_date, trend = trend, slope=slope,
             shortTerm = shortTerm,
             fPeak=fPeak,
             show.plot=show.plot) #"01-01"

  #gtp$data <- rep(20, 366)


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
  ##plot(loc_N$x, loc_N$y)
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


  #This is the second option
  #-----------------------
  if(shortTerm == "cyclical"){
    #add idx
    stp_All_bk <- stp_All %>%
      rownames_to_column('ID') #%>% #add row as column

    samp_idx <- as.numeric(sample(stp_All_bk$ID,
                                  size = round(0.05 * nrow(stp_All_bk), digits=0),
                                  replace = FALSE)) #%>
    #select 5%
    stp_All_ <- stp_All_bk[samp_idx, ]

    #regenerate IDs
    stp_All_ <-  stp_All_ %>%
      dplyr::select(-c(ID))%>%
      rownames_to_column('ID') #%

    event_Collate <- stp_All_
  }

  #-----------------------
  if(shortTerm == "acyclical"){

    #to adjust the baseline of time series
    datxy <- stp_All

    #divide the data and keep backup
    s_id <- sample(1:nrow(datxy), round(nrow(datxy)*0.75, digits = 0), replace=FALSE)
    div_75 <- datxy[s_id, ]

    div_25 <- datxy[!1:nrow(datxy)%in%s_id,]

    datxy_plot <- div_75 %>%
      #datxy_plot <- stp_All_sub %>%
      dplyr::mutate(t = as.Date(substr(datetime, 1,10)))%>%
      group_by(t) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      mutate(time = as.numeric(difftime(t, as.Date("2020-12-31"), units="days")))%>%
      as.data.frame()

    time <- data.frame(time=1:365)

    datxy_plot <- time %>%
      left_join(datxy_plot) %>%
      dplyr::mutate(n = replace_na(n, 0))

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

    #-----------------------------------------------
    #integrate the spatiotemporal sign.
    #-----------------------------------------------
    event_Collate <- NULL

    fN_final_dt_convert <- NULL

    #loop by origin
    ori_sn <- unique(filtered_stp_All$locid)[order(unique(filtered_stp_All$locid))]

    init_n <- 0

    for(or in seq_len(length(ori_sn))){ #or=1
      ##while(init_n <= n_events * 2) {

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
      t_st_List <- t_band

      #maximize the occurence of t threshold
      dt_conver_Wthres <- dt_convert %>%
        tibble()%>%
        dplyr::filter(distVal %in%  t_st_List) %>% #[1]
        dplyr::rename(distVal1 = distVal)

      #apply distance threshold
      xy <- data.frame(x=sample_sub_Dat$x, y=sample_sub_Dat$y)#[1:10,]

      ds <- dist(xy)

      ds_convert <- matrixConvert(ds, colname = c("cname", "rname", "distVal"))

      #get s threshold
      nm <- s_band

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

      flush.console()
      print(st)
      print(nrow(event_Collate))

      event_Collate <- event_Collate %>%
        dplyr::filter(!duplicated(locID_sub))

      init_n <- nrow(event_Collate)

      flush.console()
      print(or)
      print(init_n)

    }
  }

  #------------------------------------------.......#
  stp_All_bk <- event_Collate
  #------------------------------------------.......#

  #finalizing!
  #--------------------------------------#stp_All

  #generate all the results
  for(h in seq_len(length(n_events))){ #h<-1

    #Also if the final list is very small compare
    #to the list needed
    if(nrow(stp_All_bk) < round(n_events[h]*1.5, digits = 0)){
      cat(paste0("*------------| A total of ",  round(nrow(stp_All_bk)*0.75, digits = 0),
                 " events are generated! |--------------*"))

      n_events_h <- round(nrow(stp_All_bk)*0.75, digits = 0)

      #sample to derive required number
      samp_idx <- as.numeric(sample(1:nrow(stp_All_bk), size = n_events_h,
                                    replace = FALSE, prob = stp_All_bk$prob)) #%>
    }

    if(nrow(stp_All_bk) >= round(n_events[h]*1.5, digits = 0)){
      #sample to derive required number
      samp_idx <- as.numeric(sample(1:nrow(stp_All_bk), size = n_events[h],
                                    replace = FALSE, prob = stp_All_bk$prob)) #%>

    }

    stp_All_fn <- stp_All_bk[samp_idx, ]

    #sort
    stp_All_fn <- stp_All_fn %>%
      arrange(locid, tid, sn) #%>%

    output[h] <- list(stp_All_fn)
  }

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
