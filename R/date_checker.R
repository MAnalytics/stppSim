#' @title Date (Format) Checker
#' @description Checks if date is in the correct
#' format.
#' @param x A vector of date values
#' @usage date_checker(x)
#' @examples
#' date_list_1 <- c("2021-09-12", "2016-xx-02",
#' "09/08/2012")
#' date_checker(date_list_1)
#' #> FALSE (Entries 2 and 3
#' #are incorrect date inputs)
#' date_list_2 <- c("2021-09-12", "1998-03-09")
#' date_checker(date_list_2)
#' #> TRUE
#' @details Returns \code{"TRUE"} if all
#' entries of a vector are date values and
#' \code{FALSE} if any entries of a vector
#' is not a date value. The date vector
#' needs to be in the format: `"yyyy-mm-dd"`.
#' @return Returns TRUE or FALSE
#' @export
#'

date_checker <- function(x) {

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x),
                          tz = 'UTC', format = '%Y-%m-%d'))

  is.date <- sapply(list(x), is.convertible.to.date)
  is.date <- as.character(is.date)

  if("FALSE" %in% is.date){
    return("FALSE")
  }
  if(!"FALSE" %in% is.date){
    return("TRUE")
  }

}
