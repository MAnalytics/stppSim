#' @title Boundary coordinates
#' @description Boundary coordinates of Camden
#' Borough of London
#'
#' @format A dataframe containing one variable:
#' \itemize{
#'   \item x: x coordinate
#'   \item y: y coordinate
#'     }
"poly"

#' @title Records of crimes of Camden Borough of London,
#' UK, 2021 (Source: https://data.police.uk/data/)
#' @description Data comprising 'Theft' and 'Criminal Damage'
#' records of Camden Borough of London, UK
#' for the year 2021 (Source: `https://data.police.uk/`).
#' Note: Police.uk data is aggregated at monthly
#' scale (`yyyy-mm`). But, the data provided here has been
#' disaggregated to daily scale by adding fake
#' 'daily' stamps (to give `yyyy-mm-dd`). So, caution should
#' be taken when interpreting the results based on
#' full date.
#' @format A matrix containing four variables
#' \itemize{
#'   \item x: x coordinate
#'   \item y: y coordinate
#'   \item date: date of occurence
#'   \item type: types of crime
#'     }
"camden_crimes"

#' @title Spatiotemporal point data
#' @description Example spatiotemporal point data
#' of a part of San Francisco City,
#' California, US
#'
#' @format A matrix containing three variables
#' \itemize{
#'   \item x: x coordinate
#'   \item y: y coordinate
#'   \item t: t time
#'     }
"xyt_data"

