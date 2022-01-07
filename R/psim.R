#' @title Modeling of the Global Temporal Pattern
#' @description Models the global temporal pattern (of
#' the point process) as consisting of the global linear
#' trend and the seasonality.
#' @param spo (a list or dataframe) A list of spatial boundary
#' coordinates (or shapefile) within which the events are confined.
#' Should be generated using `random_spo` or `constrained_spo`
#' function.
#' @param start_date The start date of the study period.
#' Default value is `"01-01"` (i.e. January 1st). By default
#' the end date of the study period is set as `"12-31"` (i.e.
#' 31st December). A user can specify any start date in the
#' format `"mm/dd"`. The end date is the next 365th day
#' from the specified start date.
#' @param s_threshold (numeric) Spatial threshold value. The
#' (assumed) spatial range within which events are
#' re-generated (or repeated) by or around the same origin.
#' Default: \code{250} (in the same linear unit as the `poly`)
#' Default:\code{"daily"}. Other values are:
#' \code{"weekly", "monthly"}.
#' @param st_skewness (numeric) The tightness of events in space and time.
#' The value ranges from \code{0 - 1}, with event
#' volume being skewed towards the dominant origins, as the value tends
#' to \code{1}. Default: \code{0.5}. This index also controls the
#' total volume of events across space and time.
#' @param step_length (numeric) A maximum step taken at a time
#' by a walker from one state to the next. Should be a fraction
#' of the spatial units of the landscape. Default: half the size
#' of the minimum spatial unit in a landscape
#' (for a constraint landscape) or
#' @param poly (as `spatialPolygons`, `spatialPolygonDataFrames`, or
#' `simple features`). A spatial polygon defining the boundary
#' within which events are to be generated.
#' @param show.data (TRUE or FALSE) To show the output data
#' Default is \code{FALSE}.
#' @param trend (a character) Specifying the direction of
#' the global (linear) trend of the point process. Three options
#' available are `"decreasing"`, `"stable"`,
#' and `"increasing"` trends. Default: `"stable"`.
#' @param slope (a character) Slope angle for an
#' "increasing" or "decreasing" trend. Two options
#' are available: `"gentle"` and `"steep"`.
#' Default value is \code{"NULL"} for the default trend
#' (i.e. `stable`).
#' @param first_s_peak Number of days before the first seasonal
#' peak. Default: \code{90}. This implies a seasonal cycle
#' of 180 days.
#' @param npoints (an integer) Number of origins (points) to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom data.table rbindlist
#' @importFrom SiMRiv resistanceFromShape
#' @importFrom raster raster extent
#' @importFrom sp proj4string
#' @importFrom terra crs res linearUnits
#' @importFrom dplyr mutate bind_rows
#' @export
#'

psim <- function(spo, start_date, s_threshold=50, st_skewness = 0.5, step_length = 20, poly=camden_boundary, show.data = TRUE, trend, slope, first_s_peak,
                npoints, p_ratio){

  #define global variables
  x <- y <- NULL

  #spo <- random_spo(poly, npoints=50, p_ratio, show.plot=TRUE)#deal with showing plot later

  #test spo object class
  if(spo$Class != "spo"){
    stop("The 'spo' object is NOT an 'spo' Class!")
  }

  #next, extract from the spo, the N x 2 matrix or dataframe giving the
  #coordinates of the event origins (i.e. initial coordinates of the
  #simulation)
  coords <- spo$origins %>%
    select(x, y)

  #simulate the global temporal pattern
  gtp <- gtp(start_date = "01-01", trend = "stable",
      slope = "NULL", first_s_peak=90, show.plot =FALSE)


  #-----
  poly_tester(poly)
  #-----

  #check if spo and poly covers the same location (or overlap
  #each other)


  #spo <- spo[2,]

  n = gtp$data

  stp_All <- NULL

  #to implement parallelizing later

  #loop though each location and simulate point
  for(loc in 1:length(spo$origins$OriginType)){ #loc<-2
    pp_allTime <- lapply(n, function(n)
      walker(n, s_threshold = s_threshold,
             poly=poly, coords=c(spo$origins$x[loc],spo$origins$y[loc]),
                      step_length = 20,
                      show.plot = FALSE))

    #collapse list
    pp_allTime <- rbindlist(pp_allTime,
                            use.names=TRUE, fill=TRUE, idcol="tid")

    # if(loc==1){
    #   bk_ <- pp_allTime
    # }

    #append location id
    pp_allTime <- pp_allTime %>%
      mutate(locid=loc, prob=spo$origins$prob[loc]) #%>%
      #append location id and pareto prob
      #mutate(x = spo$origins$x[loc] + x, y = spo$origins$y[loc] + y)#update coordinates

    stp_All <- stp_All %>%
      bind_rows(pp_allTime)


    flush.console()
    print(loc)
  }


  #select a uniform % from each group
  mtcars %>%
    arrange(cyl)%>%
    group_by(cyl) %>%
    mutate(n=n())%>%
    data.frame()%>%
    group_by(cyl)%>%
    slice_sample(prop = 0.5, replace = FALSE) %>%#select 50%
    arrange(cyl)

  #spatial and temporal tightness
  stp_All %>%
    group_by()



}

# dev.new()
# kk <- stp_All[1:500000, c(3:4)]
# kk <- bk_[1:20, c(3:4)]
#
#   plot(kk, type="l", asp=1, col="gray80")
#   points(kk, col="red")
#   ext <- extract_coords(camden_boundary)
#   points(ext, col="black")
#   text(sim_events_[,1], sim_events_[,2],
#        labels=sim_events_[,4], cex= 0.7, pos=3)


#spatial and temporal tightness
stp_All_bb <- stp_All

#stp_All_bb %>%

library(dplyr)

group<-c(1,1,1,1,2,2,2)
prob<-c(0.22,0.22,0.1,0.11,0.24,0.32,0.93)
var1<-c('aa','ab','ac','ba','bb','ca','ce')
var2<-c('aaa','aba','aca','baa','bba','caa','cba')
var3<-c('aab','abb','acb','bab','bbb','cab','ceb')
data<-data.frame(group,prob,var1,var2,var3)
data


mtcars %>%
  group_by(cyl) %>%
  do(sample_n(.,2))


data %>%
  group_by(group)%>%
  mutate(n=n())%>% #per grp
  filter(sample_n(data, size = 5, weight = group))







probs <- data.frame(
  group=rep(c(1,2,3), each=4),
  metric=rep(rep(c("A", "B"), each=2), each=1),
  measurement=rep(c("HI", "LO"), 6),
  probability=c(0.8,0.2,0.5,0.5,0.7,0.3,0.4,0.6,0.1,0.9,0.05,0.95)
)


probs %>%
  tibble::rownames_to_column('ID')%>%
  mutate(ids=paste(group, ID, sep=""))%>%
  group_by(group) %>%
  mutate(sample(ids, size = 2, replace = TRUE, prob = probability)) #





data <- data.frame(
  group=sample(c(1,2), size=12, replace=TRUE),
  metric=sample(c("A", "B"), replace=TRUE, size=12),
  measurement=NA  # To be sampled
)


probs %>%
  group_by(group,metric) %>%
  mutate(sim_meas = sample(measurement, size = 1, replace = TRUE, prob = probability)) #





#solutionl: function that sample by group, by metric
sim_meas <- function(x,y){
  bb <- probs %>%
    group_by(group,metric) %>%
    mutate(sim_meas = sample(measurement, size = 1, replace = TRUE, prob = probability)) #%>%
    #filter(group == x & metric == y)
  return(bb$sim_meas[1])
}

data$measurement <- apply(data,1,function(x) sim_meas(x[1],x[2]))


