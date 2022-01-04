#' @title Modeling of the Global Temporal Pattern
#' @description Models the global temporal pattern (of
#' the point process) as consisting of the global linear
#' trend and the seasonality.
#' @param start_date The start date of the study period.
#' Default value is `"01-01"` (i.e. January 1st). By default
#' the end date of the study period is set as `"12-31"` (i.e.
#' 31st December). A user can specify any start date in the
#' format `"mm/dd"`. The end date is the next 365th day
#' from the specified start date.
#' @param t_resolution (character) The temporal resolution
#' at which events are re-generated (or repeated). Specified in
#' number of days. Default:\code{1} (currently the only option
#' available).
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
#' @param scale (an integer) For scaling point counts. Default: \code{1}
#' @param show.plot (TRUE or False) To show the time series
#' plot. Default is \code{FALSE}.
#' @usage gtp(start_date = "01-01", trend = "stable",
#' slope = "NULL", first_s_peak=90, scale = 1, show.plot =FALSE)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @export
#'

gtp <- function(start_date="01-01", trend="stable",
                slope = "NULL", first_s_peak = 90,
                t_resolution = 1, show.plot =FALSE){

  output <- list() #output object

  #check the t resolution
  if(t_resolution != 1){
    stop("'t_resolution' needs to be '1'")
  }

  #prepare date
  t1 <- as.Date(paste("2021", start_date, sep="-"))
  t <- seq(0, 365, by = 1)
  t2 <- t1 + t

  y <- 20 * cos(3 + 2 * pi * t/(2 * first_s_peak)) + 0.2 * sin(-1 * pi * t/15)

  y <- y #* scale

  y <- (y + (-1 * min(y)))  #to remove negatives values and scale

  #if trend is 'stable', slope has to be 'NULL'
  if(trend == "stable"){
    if(slope != "NULL"){
      stop("Slope needs to be 'NULL' for a stable trend!")
    }
  }

  gentle <- ((max(y)/2) - min(y))/(365-0) #slope
  steep <-  ((max(y)) - min(y))/(365-0) #slope

  if(trend == "decreasing"){
    if(slope == "NULL"){
      stop("Slope cannot be NULL for a decreasing trend!")
    }
    #check slope
    if(slope == "gentle"){
      gentle <- -gentle
      trendline <- 0 + gentle * t
    }
    if(slope == "steep"){
      steep <- -steep
      trendline <- 0 + steep * t
    }

    y <- y + trendline

  }

  if(trend == "increasing"){

    if(slope == "NULL"){
      stop("Slope cannot be NULL for an increasing trend!")
    }

    if(slope == "gentle"){
      trendline <- 0 + gentle * t
  }
    if(slope == "steep"){
      trendline <- 0 + steep * t
    }

    y <- y + trendline
  }


  #remove negative values
  y <- round(y + (-1 * min(y)), digits = 0) #to remove negatives values
  baseline_occur <- 0.25 * (max(y)/2)
  y <- round(y + baseline_occur, digits = 0) #add baseline
  y_ <- data.frame(Date = t2, y)


  output$data <- y

  #output$plot <- plot(t, y, 'l')

  if(show.plot == TRUE){
  flush.console()
  plot(t, y, 'l')
  }

  return(output)

}
