#' @title Date Format Checker
#' @description Checks if date is in the correct format, i.e.
#' `yyyy-mm-dd`
#' @param x A date input
#' @param format The date format required.
#' Default value: \code{"%Y-%m-%d"}.
#' @usage is_date(x, format="%Y-%m-%d")
#' @examples
#' @details Returns TRUE or FALSE, to indicate
#' that the date input is in the correct or incorrect
#' format, respectively
#' @return Returns TRUE or FALSE
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @export
#'

date_checker <- function(x, format = "%Y-%m-%d") {

  formatted = try(as.Date(x, format), silent = TRUE)
  is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
  is_date[is.na(x)] = NA  # Insert NA for NA in x

  return(is_date)

}
