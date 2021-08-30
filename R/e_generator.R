#' @title Event (point) generators
#' @description Generates completely random points inside
#' a polygon boundary and assign probability values (signifying
#' their strength) in accordance with
#' the pareto principle (i.e. 80% of the events
#' come from 20% of the locations).
#' @param poly (a list or dataframe) A list of boundary coordinates
#' (cartesian or geographic) of a study area
#' @param npoints (an integer) Number of points to generate
#' @usage crp(poly, npoints)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @export
#'

crp <- function(poly, npoints =  50){

  flush.console <- NULL
  #generate random pints inside the boundary
  ran_points <- csr(poly, npoints)

  #assign prob. values in accord. with pareto
  #(x/10 * sqrt(A) = x') (Courtesy: Odekadzo)
  prob_values <- p_prob(npoints=npoints)

  #ran_points_prob <-



}


