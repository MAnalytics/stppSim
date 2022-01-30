#' @title Learning the spatiao-temporal properties of sample data
#' @description Learns all the spatial and temporal
#' properties (required to build events' population)
#' from a sample points.
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
#' sample_size <- 500
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
stp_learner <- function(ppt, start_date = NULL, poly = NULL, crsys = "CRS_string"){

  #global var
  grid_id <- count <- NULL
  origins <- list()

  #check if start_date is supplied, if not
  #extract from the point data.
  if(is.null(start_date)){
    #min time
    min_t <- min(as.Date(ppt[,3]))

    ppt_df <- data.frame(ppt)
    colnames(ppt_df) <- c("x","y","t")

    #check the date field too
    t_format <- length(which(date_checker(c(ppt[,3]),
                                          format = "%Y-%m-%d")==FALSE))
    #check time column
    if(t_format > 0){
      stop("At least one entry of the 'time' column is not in the correct format!")
    }

    #ensuring data does not exceed 1-year length
    if(as.numeric(difftime(max(ppt_df$t), min(ppt_df$t), units="days")) > 365){
      stop(paste("Data length is greater than 365 days!",
                 "Less than or equal to 1-year data length is required!"))
    }


  } else{
    if(date_checker(c(start_date), format = "%Y-%m-%d") == FALSE){
      stop("The 'start_date' specified is not in the correct format!")
    }

    #check the date field too
    t_format <- length(which(date_checker(c(ppt[,3]),
                                   format = "%Y-%m-%d")==FALSE))
    #check time column
    if(t_format > 0){
      stop("At least one entry of the 'time' column is not in the correct format!")
    }

    #compared start time with min time
    min_t <- min(as.Date(ppt[,3]))
    start_date <- as.Date(start_date)

    min_compared <- length(which(as.Date(ppt[,3]) < start_date))

    if(min_compared > 0){
      stop(paste("One or more entry(s) in the time column of 'ppt'",
          "is less than the 'start_date'! Consider setting 'start_date = NULL'!", sep=" "))
    }

    ppt_df <- data.frame(ppt)
    colnames(ppt_df) <- c("x","y","t")

    min_x <- min(ppt_df$x)
    min_y <- min(ppt_df$y)

    min_xyt <- data.frame(x=min_x,
                                y=min_y,
                                t=start_date) #start_date = "2014-12-31"
    #combine
    ppt_df <- data.frame(rbind(min_xyt, ppt_df))

    max_t <- max(ppt_df$t)

    #diff between min and max
    #ensuring data does not exceed 1-year length
    if(as.numeric(difftime(max(ppt_df$t), min(ppt_df$t), units="days")) > 365){
      stop(paste("Data length is greater than 365 days!",
                 "Less than or equal to 1-year data length is required!"))
    }

  }

  #Now, learn the temporal pattern and trend from
  #the sample dataset

  dat_sample_p <- ppt_df %>%
      group_by(t) %>%
      summarise(n = n())%>%
      mutate(time = round(as.numeric(difftime(t, as.Date(min(t)), units="days")),
             digits = 0))%>%
      as.data.frame()

    #create a sub table
    time <- data.frame(time=1:365)

    dat_sample_p <- time %>%
      left_join(dat_sample_p) %>%
      dplyr::mutate(n = replace_na(n, 0))

    #unique(dat_sample_p$time)

    #plot(dat_sample_p$time, dat_sample_p$n, 'l')
    #head(datxy)

    #--------------------------------
    #smoothen and plot
    loessData <- data.frame(
      x = 1:nrow(dat_sample_p),
      y = predict(loess(n~time, dat_sample_p, span = 0.3)),
      method = "loess()"
    )

    #scaling by a factor of 10
    #for high intensity data
    s_factor <- 10
    #--------------------
    loessData$y <- loessData$y * s_factor
    #--------------------

    #----------------------------------
    #learn the spatial pattern
    #import square grid
    #----------------------------------
    #check if boundary is supplied,
    #if null, create an arbitrary boundary
    if(is.null(poly)){
      ppt_xy <-
        matrix(as.numeric(ppt[,1:2]),,2)
      #create boundary from points
      boundary_ppt <- chull_poly(ppt_xy)
      #then define the crs
      if(is.null(crsys)){
        stop(paste("The 'crsys' argument cannot be NULL",
             "while 'poly' argument is also NULL!!",
             "Needs to define the 'crsys' argument", sep=" "))
      } else {
        proj4string(boundary_ppt) <- CRS(crsys)
        #proj4string(boundary_ppt) <-
        #"+proj=utm +zone=10 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs"
      }

    } else {

      if(is.na(crs(poly))){
        stop("The 'poly' object has no projection, i.e. crs")
      }

      #first check that 'poly' has
      #a projection
      if(!is.null(crsys)){
        flush.console()
        print(paste("Warning: The projection system (crs) of 'poly'",
              "object is utilized!!", sep=" "))
      }
      if(is.na(crs(poly))){
        stop(paste("'poly' object must have a projection!"))
      }
      boundary_ppt <- poly
    }

    #determine if projection is in metres or feet
    #if metres, use 500m2
    #if feet, 5000ft2
    s4_proj <- proj4string(boundary_ppt)

    if(grepl("+units=m", s4_proj, fixed = TRUE) == TRUE){
      grid_size <- 500
    }
    if(grepl("+units=us-ft", s4_proj, fixed = TRUE)==TRUE){
      grid_size <- 1700
      }

    if((grepl("+units=m", s4_proj, fixed = TRUE) == FALSE)&
       (grepl("+units=us-ft", s4_proj, fixed = TRUE)==FALSE)){
      stop(paste("Specified 'crs' not recognized!",
                 "A 'Metre-based' projection is preferred", sep=" "))
    }

    #create regular grids
    #default 250 square metres
    set.seed(1000)
    grid_sys <- make_grids(poly=boundary_ppt, size = grid_size, show_output = FALSE,
               dir=NULL)
    grid_sys$grid_id <- 1:length(grid_sys)

    #plot(grid_sys)

    #Now overlay points on the grids
    ppt_df <- ppt_df %>%
      rownames_to_column("id")

    #id <- as.numeric(ppt_df$id)
    x <- as.numeric(ppt_df$x)
    y <- as.numeric(ppt_df$y)
    t <- as.Date(ppt_df$t)
    xyid_ <- cbind(x, y)
    #ppt_df$x <- as.numeric(ppt_df$x)
    xyid_ppt <- SpatialPoints(xyid_)
    proj4string(xyid_ppt) <- crs(grid_sys)
    #plot(xyid_ppt, add=TRUE)

    grid_sys <- st_as_sf(grid_sys)
    xyid_ppt <- st_as_sf(xyid_ppt)
    #convert
    pnt_grid_intsct <- st_intersects(xyid_ppt, grid_sys, sparse = TRUE)
    pnt_grid_intsct <- data.frame(pnt_grid_intsct)
    colnames(pnt_grid_intsct) <- c("id", "grid_id")
    pnt_grid_intsct$id <- as.character(pnt_grid_intsct$id)

    #join
    #Assign the probability value to each grid
    #based on its historical events
    ppt_df_join_count <- ppt_df %>%
      left_join(pnt_grid_intsct)%>%
      group_by(grid_id) %>%
      summarise(count = n()) %>%
      arrange(desc(count))%>%
      mutate(prob = round(count/sum(count),
                          digits=10))


    #Now arrange the result how I want it to
    #look, for spo
    #nam convention..









    # #extract centroid coord of grids
    # grid_sys_xy <- cbind(grid_sys$grid_id,
    #                      extract_coords(grid_sys))
    #
    #
    # #Assign the probability value
    # #based on pareto ratio
    # grid_sys %>%
    #   leppt_df_join_count
    #
    # origins$origins <- ran_points_prob
    # origins$plot <- p
    # origins$poly <- backup_poly
    # origins$Class <- "artif_spo"
    #
    # origins$gtp <-
    # origins$gtp <-
    # origins$Class <- "real_spo"
    #NEXT

}
  #
  # date_checker(as.Date("2000-01-01"))

  #..then(create square grid)
  #learn spatial properties
  #1.
  #2. derive spatial origins
  #impose square grids
  #is boundary supplied yes
  #if no create chull poly..

#}