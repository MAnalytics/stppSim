#' @title Date (Format) Checker
#' @description Checks if date is in a
#' specified format (i.e. `'yyyy-mm-dd'`).
#' @param x A date or a vector of date values
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
#' date entries are in the specified format
#' (`"yyyy-mm-dd`),
#' and \code{FALSE} if at least one date is not
#' in the format.
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
