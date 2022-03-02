#' @title Global temporal pattern (GTP)
#' @description Models the global temporal pattern,
#' as combining long-term trend and seasonality.
#' @param start_date The start date of temporal pattern.
#' The date should be in the format `"yyyy-mm-dd"`.
#' The GTP will usually covers a 1-year period.
#' @param trend (string) Specify the trend direction of
#' the GTP. Values are: `"decreasing"`, `"stable"`,
#' and `"increasing"`. Default is: `"stable"`.
#' @param slope (string) Slope GTP trend if
#' "increasing" or "decreasing" trend is specified.
#' Values: `"gentle"` or `"steep"`.
#' Default value is \code{NULL} (i.e., for `stable` trend).
#' @param first_pDate (in `"yyyy-mm-dd"` format).
#' Date of the
#' first seasonal peak of the time series.
#' Default value is \code{NULL}, in which a
#' seasonal cycle of 180 days is utilized. That is,
#' a first seasonal peak of 90 days.
#' @param show.plot (logical) Shows GTP.
#' Default is \code{FALSE}.
#' @usage gtp(start_date, trend = "stable",
#' slope = NULL, first_pDate = NULL, show.plot =FALSE)
#' @examples
#' gtp(start_date = "2020-01-01", trend = "stable",
#' slope = NULL, first_pDate = "2020-02-28", show.plot = FALSE)
#' @details Models the GTP which could be utilize for simulating
#' artifical point pattern across space.
#' @return Returns a vector of 365 data points representing
#' the global temporal pattern
#' @export
#'

gtp <- function(start_date = "yyyy-mm-dd", trend = "stable",
                slope = NULL, first_pDate=NULL,
                show.plot = FALSE){

  #function to check if start_date & first_pDate are
  #in correct format

  #check that start_date has value
  if(start_date == "yyyy-mm-dd"){
    stop("Error! 'start_date' argument has to be a real date!")
  }

  #check first peak value
  if(is.null(first_pDate)){
    first_pDate <- as.Date(start_date) + 90
  }

  if(date_checker(c(start_date)) == FALSE){
    stop("The 'start_date' specified is not in the correct format!")
  }

  if(date_checker(c(first_pDate)) == FALSE){
    stop("The 'first_pDate' specified is not in the correct format!")
  }

  #check if first_pDate is greater than start date
  if(as.numeric(as.Date(first_pDate) - as.Date(start_date)) <= 0){
    stop("The 'start_date' cannot be a later date than 'first_pDate' ")
  }


  output <- list() #output object

  #prepare date
  t1 <- as.Date(start_date)
  t <- seq(0, 365, by = 1)
  t2 <- t1 + t

  #n-th day of peak since start_date
  nth_day <- as.numeric(as.Date(first_pDate) - as.Date(start_date))

  y <- 20 * cos(3 + 2 * pi * t/(2 * nth_day)) + 0.2 * sin(-1 * pi * t/15)

  y <- y #* scale

  y <- (y + (-1 * min(y)))  #to remove negatives values and scale

  #if trend is 'stable', slope has to be 'NULL'
  if(trend == "stable"){
    if(!is.null(slope)){
      stop("Slope needs to be 'NULL' for a stable trend!")
    }
  }

  gentle <- ((max(y)/2) - min(y))/(365-0) #slope
  steep <-  ((max(y)) - min(y))/(365-0) #slope

  if(trend == "decreasing"){
    if(is.null(slope)){
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

    if(is.null(slope)){
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
