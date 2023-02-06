#' @title Global temporal pattern (GTP)
#' @description Models the global temporal pattern,
#' as combining the long-term trend and seasonality.
#' @param start_date the start date of the temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The GTP will normally cover a 1-year period.
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
#' @usage gtp(start_date, trend = "stable",
#' slope = NULL, shortTerm = "cyclical",
#' Tperiod = NULL, show.plot =FALSE)
#' @examples
#' gtp(start_date = "2020-01-01", trend = "stable",
#' slope = NULL, shortTerm = "cyclical",
#' Tperiod = 60, show.plot = FALSE)
#' @details Models the GTP for anchoring the temporal
#' trends and patterns of the point patterns to be simulated.
#' @return Returns a time series (list) of 365
#' data points representing
#' 1-year global temporal pattern.
#' @export
#'

gtp <- function(start_date = "yyyy-mm-dd", trend = "stable",
                slope = NULL, shortTerm = "cyclical",
                Tperiod=NULL,
                show.plot = FALSE){

  #function to check if start_date & Tperiod are
  #in correct format

  #check that start_date has value
  if(start_date %in% "yyyy-mm-dd"){
    stop("Error! 'start_date' argument has to be a real date!")
  }

  #check first peak value
  if(is.null(Tperiod)){
    first_pDate <- as.Date(start_date) + 90
    Tperiod <- 90
  }

  if(!is.null(Tperiod)){
    first_pDate <- as.Date(start_date) + Tperiod
  }

  if(date_checker(c(start_date)) == FALSE){
    stop("The 'start_date' specified is not in the correct format!")
  }

  # if(date_checker(c(first_pDate)) == FALSE){
  #   stop("The 'first_pDate' specified is not in the correct format!")
  # }

  #check if first_pDate is greater than start date
  if(as.numeric(as.Date(first_pDate) - as.Date(start_date)) <= 0){
    stop("The 'start_date' cannot be a later date than 'first_pDate' ")
  }


  output <- list() #output object

  #prepare date
  t1 <- as.Date(start_date)
  t <- seq(0, 365, by = 1)
  t2 <- t1 + t


  if(shortTerm == "cyclical"){

    #n-th day of peak since start_date
    nth_day <- as.numeric(as.Date(first_pDate) - as.Date(start_date))

    y <- 20 * cos(3 + 2 * pi * t/(2 * nth_day)) + 0.2 * sin(-1 * pi * t/15)

    y <- y #* scale

    y <- (y + (-1 * min(y)))  #to remove negatives values and scale
  }


  if(shortTerm == "acyclical"){
    y <- rep(60, length(t))
  }


  #if trend is 'stable', slope has to be 'NULL'
  if(trend == "stable"){
    if(!is.null(slope)){
      stop("Slope needs to be 'NULL' for a stable trend!")
    }
  }

  if(shortTerm == "cyclical"){
    gentle <- ((max(y)/2) - min(y))/(365-0) #slope
    steep <-  ((max(y)) - min(y))/(365-0) #slope
  }

  if(shortTerm == "acyclical"){
    gentle <- 0.05#slope
    steep <-  0.1 #slope
  }


  if(trend == "falling"){
    if(is.null(slope)){
      stop("Slope cannot be NULL for a falling trend!")
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
    #plot(y)
  }

  if(trend == "rising"){

    if(is.null(slope)){
      stop("Slope cannot be NULL for an rising trend!")
    }

    if(slope == "gentle"){
      trendline <- 0 + gentle * t
  }
    if(slope == "steep"){
      trendline <- 0 + steep * t
    }

    if(shortTerm == "acyclical"){
      y <- y - 30
    }
    y <- y + trendline
    #plot(y)
  }

  if(shortTerm == "cyclical"){
    #remove negative values
    y <- round(y + (-1 * min(y)), digits = 0) #to remove negatives values
    baseline_occur <- 0.25 * (max(y)/2)
    y <- round(y + baseline_occur, digits = 0) #add baseline
    y_ <- data.frame(Date = t2, y)
  }


  output$data <- y
  output$Tperiod <- Tperiod
  #output$plot <- plot(t, y, 'l')

  if(show.plot == TRUE){
  flush.console()
  plot(t, y, 'l')
  }

  return(output)

}
