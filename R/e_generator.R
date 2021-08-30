#' @title Event (point) generators
#' @description Generates completely random points inside
#' a polygon boundary and assign probability values (signifying
#' their strength) in accordance with
#' the pareto principle (i.e. 80% of the events
#' come from 20% of the locations).
#' @param poly (a list or dataframe) A list of boundary coordinates
#' (cartesian or geographic) of a study area
#' @param npoints (an integer) Number of points to generate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a 20:80
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Input values must be \code{10}, \code{20},
#' \code{30}, \code{40}, or \code{50}. The 'p_ratio'
#' determines the proportion of points that are the most
#' dominant event generators.
#' @usage e_generator(poly, npoints, p_ratio)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @export
#'

e_generator <- function(poly, npoints =  50, p_ratio = 30){

  flush.console <- select <- prob <- NULL
  #generate random pints inside the boundary
  ran_points <- as.data.frame(csr(poly, npoints))
  colnames(ran_points) <- c("x", "y")
  #points(ran_points$x,ran_points$y)

  #assign prob. values in accord. with pareto
  #(x/10 * sqrt(A) = x') (Courtesy: Odekadzo)
  prob_values <- p_prob(npoints=npoints, p_ratio = p_ratio)
  #check
  #sum(prob_values[1:round(npoints*.8, digits=0),4])
  #sum(prob_values[round(npoints*.8, digits=0):nrow(prob_values),4])

  #append pareto prob. values to random points
  ran_points_prob <- data.frame(cbind(ran_points,
                          prob=prob_values%>%select(prob)))

  #Given event count at a temporal bin,
  #simulate walkers to generate the number of event
  #count

  return(ran_points_prob)
}


