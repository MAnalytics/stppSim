#' @title Simulate spatial point origins constrained
#' by the social configuration of the urban space.
#' @description Simulate event origins (EOs) on a land use map
#' (contrained space) with binary classes \code{1} and \code{0}, representing
#' enabled and disabled origins. An enabled origin can
#' generate events while disabled origins can not generate
#' events. Each enabled origin is assigned
#' a probability value (representing the intensity) at which
#' the origin generates events in accordance with a specified
#' Pareto ratio.
#' @param bpoly (a spatialPolygonDataFrames) with binary attribute
#' field `class` with values \code{1} and \code{0}, representing
#' the enabled and disabled origins.
#' @param npoints (an integer) Number of points to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' points (origins).
#' @usage nRandom_spo(bpoly, npoints =  50, p_ratio = 30,
#' show.plot=FALSE)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @importFrom grDevices chull
#' @export
#'

nRandom_spo <- function(bpoly=camden_boundary, npoints =  50, p_ratio = 30,
                          show.plot=FALSE){

    #create a boundary map from the base map
    poly2 <- st_as_sf(bpoly)
    boundMap <- st_union(poly2) #plot(boundMap)


  }
