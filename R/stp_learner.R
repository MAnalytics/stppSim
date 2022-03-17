#' @title Learning the spatiotemporal properties of
#' a sample data
#' @description Learns both the spatial and the temporal
#' properties of a real sample dataset.
#' @param ppt A 3-column matrix or list containing
#' `x` - eastings, `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd').
#' @param start_date the start date of the temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The temporal pattern will normally
#' cover 1-year period.
#' @param poly (An sf or S4 object)
#' a polygon shapefile defining the extent of the landscape
#' @param gridSize the size of square grid
#' to use for discretizing the space.
#' Default is: \code{150}.
#' @param n_origin number of locations to serve as
#' origins for walkers. Default:\code{50}.
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio.
#' For example, a value of \code{20}
#' implies a \code{20:80} Pareto ratio.
#' @param crsys (string) the EPSG code of the projection
#' system of the `ppt` coordinates. This only used if
#' `poly` argument is \code{NULL}.
#' See "http://spatialreference.org/" for the list of
#' EPSG codes for different regions of the world.
#' As an example, the EPSG code for the British National Grid
#' projection system is: \code{"EPSG:27700"}.
#' @param show.plot (TRUE or FALSE) Whether to show
#' some displays.
#' @usage stp_learner(ppt, start_date = NULL, poly = NULL,
#' n_origin=50, p_ratio, gridSize = 150,
#' crsys = NULL, show.plot = FALSE)
#' @examples
#' #Goal: To learn the ST properties
#' #of a sample data, for the purpose of
#' #simulating the full dataset (see `psim_real`).
#' data(camden_crimes)
#' #subset 'theft' crime
#' theft <- camden_crimes[which(camden_crimes$type ==
#' "Theft"),1:3]
#' #specify the proportion of full data to use
#' sample_size <- 0.3
#' set.seed(1000)
#' dat_sample <- theft[sample(1:nrow(theft),
#' round((sample_size * nrow(theft)), digits=0),
#' replace=FALSE),]
#' #plot(dat_sample$x, dat_sample$y) #preview
#' stp_learner(dat_sample,
#' start_date = NULL, poly = NULL, n_origin=50,
#' p_ratio=20, gridSize = 150, crsys = "EPSG:27700",
#' show.plot = FALSE)
#' @details Returns an object of the class `real_spo`,
#' storing details of the spatiotemporal
#' properties of the sample data learnt.
#' @return an object (list) containing specific spatial
#' and temporal properties of a sample dataset.
#' @importFrom dplyr select group_by
#' mutate summarise left_join n arrange
#' desc
#' @importFrom tidyr replace_na
#' @importFrom sp SpatialPoints proj4string
#' @importFrom stats predict loess
#' @importFrom sf st_area st_geometry st_centroid
#' @importFrom ggplot2 aes theme element_text
#' theme_light geom_sf
#' @importFrom spatstat.geom ppp owin
#' @importFrom sparr OS
#' @importFrom tibble rownames_to_column
#' @export
#'
stp_learner <- function(ppt, start_date = NULL, poly = NULL,
                        n_origin=50, p_ratio, gridSize = 150,
                        crsys = NULL, show.plot = FALSE){

  prob <- NULL

  output <- list()

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
    t_format <- length(which(date_checker(c(ppt[,3]))==FALSE))
    #check time column
    if(t_format > 0){
      stop("At least one entry of the 'time' column is not in the correct format!")
    }

    #ensuring data does not exceed 1-year length
    if(as.numeric(difftime(max(ppt_df$t), min(ppt_df$t), units="days")) > 366){
      stop(paste("Data length is greater than 365 days!",
                 "Data length has to be less than or equal to 1-year!"))
    }

    final_start_date <- min_t

  } else{
    if(date_checker(c(start_date)) == FALSE){
      stop("The 'start_date' specified is not in the correct format!")
    }

    #check the date field too
    t_format <- length(which(date_checker(c(ppt[,3]))==FALSE))
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

    final_start_date <- start_date
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
    t <- as.vector(unlist(time))

    suppressMessages(
    dat_sample_p <- time %>%
      left_join(dat_sample_p) %>%
      dplyr::mutate(n = replace_na(n, 0)))


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

    gtp <- round(loessData$y, digits = 0)
    # if(show.plot == TRUE){
    #   flush.console()
    #   plot(t, gtp, 'l')
    # }
    #--------------------

    #----------------------------------
    #learn the spatial pattern
    #import square grid
    #----------------------------------
    #check if boundary is supplied,
    #if null, create an arbitrary boundary
    if(is.null(poly)){
      ppt_xy <- ppt[,1:2]
      #create boundary from points
      boundary_ppt <- chull_poly(ppt_xy)
      #then define the crs
      if(is.null(crsys)){
        stop(paste("'crsys' argument cannot be NULL",
             "when 'poly' argument is NULL!!",
             "Needs to define the 'crsys' argument", sep=" "))
      } else {
        suppressWarnings(
        proj4string(boundary_ppt) <- CRS(crsys))
        #warning msg
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

    #calculate spatial bandwidth
    #boundary coord
    x_min <- min(extract_coords(boundary_ppt)$X)
    x_max <- max(extract_coords(boundary_ppt)$X)

    y_min <- min(extract_coords(boundary_ppt)$Y)
    y_max <- max(extract_coords(boundary_ppt)$Y)

    suppressWarnings(
    ppt_df_ppp <- ppp(x=as.numeric(ppt_df$x),
        y=as.numeric(ppt_df$y),
        window = owin(c(x_min, x_max),
                      c(y_min, y_max))))

    sbw <- OS(ppt_df_ppp)/4

    #determine if projection is in metres or feet
    #if metres, use 500m2
    #if feet, 5000ft2
    suppressWarnings(
    s4_proj <- proj4string(boundary_ppt))
    #warning

    # #determine grid size by dividing the area
    # #by n_origin #(work to do)
    # m_square <- as.numeric(st_area(st_as_sf(boundary_ppt))) / n_origin
    #
    #
    # if(grepl("+units=m", s4_proj, fixed = TRUE) == TRUE){
    #   #grid_size <- 500
    #   grid_size <- m_square^(1/2)
    # }
    # if(grepl("+units=us-ft", s4_proj, fixed = TRUE)==TRUE){
    #   #grid_size <- 1700
    #   grid_size <- m_square^(1/2)
    #   }

    #check the crs
    if((grepl("+units=m", s4_proj, fixed = TRUE) == FALSE)&
       (grepl("+units=us-ft", s4_proj, fixed = TRUE)==FALSE)){
      stop(paste("Specified 'crs' not recognized!",
                 "A 'Metre- or Feet-based' projection is preferred", sep=" "))
    }

    #create regular grids
    #default 250 square metres
    set.seed(1000)
    grid_sys <- make_grids(poly=boundary_ppt,
                           size = gridSize, show_output = FALSE)
    #warning msg

    #plot(grid_sys)
    grid_sys$grid_id <- 1:length(grid_sys)
    #plot(grid_sys)

    #Now overlay points on the grids
    ppt_df <- ppt_df %>%
      rownames_to_column("id")
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

    suppressWarnings(
    stc <- st_centroid(grid_sys))
    #warning msg
    ptsxy <- data.frame(cbind(do.call(rbind, st_geometry(stc)),
                              stc$grid_id))
    colnames(ptsxy) <- c("x","y","grid_id")

    #join
    #Assign the probability value to each grid
    #based on its historical events
    suppressMessages(
    spo <- ppt_df %>%
      left_join(pnt_grid_intsct)%>%
      group_by(grid_id) %>%
      summarise(count = n()) %>%
      arrange(desc(count))%>%
      mutate(prob = round(count/sum(count),
                          digits=10)) %>%
      left_join(ptsxy) %>%
      arrange(prob)%>%
      filter(!is.na(grid_id)))

    #now randomly select 'n_origin'
    #taking into account the 'resistance_feat'

      if(length(spo$grid_id) > n_origin){
      samp_idx <- as.numeric(sample(spo$grid_id, size = n_origin,
                        replace = FALSE, prob = spo$prob)) #%>
      spo <- spo %>%
        filter(grid_id %in% samp_idx)
      }

      if(length(spo$grid_id) <= n_origin){
        spo <- spo #do nothing
      }

    #Note: 'resistance_feat' has not use here
    #but could be added (optionally) to the simulation function
    #if a user deems fit.


    x <- as.numeric(spo$x)
    y <- as.numeric(spo$y)

    spo_xy <- data.frame(cbind(x,y))
    colnames(spo_xy) <- c("x","y")

    output$origins <- spo
    output$gtp <- gtp
    output$start_date <- start_date
    output$s_threshold <- sbw
    #output$plot <- p
    output$poly <- boundary_ppt
    output$Class <- "real_spo"

    output$origins <- spo


    return(output)

}

