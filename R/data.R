#' @title Boundary Coordinates
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
#' @description 'Theft' and 'Criminal Damage' records
#' (Source: `police.uk`)
#' of Camden Borough of London, UK
#' for the year 2021.
#' Note: Police.uk data is only accurate to monthly
#' scale (`yyyy-mm`). But, the data provided here has been
#' manipulated to include fake 'days of occurrence'.
#' So, caution should
#' be taken when interpreting the results based on
#' full date stamp (`yyyy-mm-dd`).
#' @format A matrix containing three variables
#' \itemize{
#'   \item x: x coordinate
#'   \item y: y coordinate
#'   \item date: date of occurence
#'   \item type: type of crime
#'     }
"camden_crimes"

#' @title A boundary shapefile
#' @description A boundary shapefile of Camden Borough, London, UK
#' @format A boundary file (ESRI format)
#' \itemize{
#'   \item x: x coordinate
#'   \item y: y coordinate
#'     }
"camden_boundary"

#' @title Landuse shapefile
#' @description A land use shapefile of Camden
#' Borough of London, United Kingdom
#' @format A boundary file (ESRI format)
#' \itemize{
#'   \item type: Landuse type
#'   \item rValues1: Field specifying a uniform resistance
#'   for all features (Value = 1)
#'   for all features
#'   \item rValues2: Field specifying varying resistance
#'   values for different feature classes.
#'     }
"landuse"

#' @title Spatiotemporal point data
#' @description A spatiotemporal point data
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

