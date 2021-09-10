#' @title Simulate non-random event origins
#' @description Simulate event origins (EOs) on a land use map
#' with binary classes \code{0} and \code{1}, representing
#' non-active and active origins. Each origin is assigned
#' a probability value (signifying their respective strength
#' to generate events). The EOs are marked in accordance
#' with a specified Pareto ratio. The extent of the land use map
#' serves as the boundary of the area.
#' @param bpoly (a spatialPolygonDataFrames) with attribute
#' field `class` indicating \code{0} for non-event origin, and
#' \code{`1`} for event origin.
#' @param npoints (an integer) Number of points to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' points (origins).
#' @usage non_random_spp(bpoly, npoints =  50, p_ratio = 30,
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

non_random_spp <- function(bpoly=camden_boundary, npoints =  50, p_ratio = 30,
                          show.plot=FALSE){

    #create a boundary map from the base map
    poly2 <- st_as_sf(bpoly)
    boundMap <- st_union(poly2) #plot(boundMap)


  }
