#' @title Simulate random spatial point patterns
#' @description Simulate spatial point pattern from
#' randomly created event origin. Each origin is assigned
#' a probability value (representing the intensity) at which
#' the origin generates events in accordance with a specified
#' Pareto ratio.
#' @param poly (a list or dataframe) A list of spatial boundary
#' coordinates within which the events are confined.
#' @param npoints (an integer) Number of points to simulate
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' points (origins).
#' @usage random_spp(poly, npoints, p_ratio, show.plot=FALSE)
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

random_spp <- function(poly, npoints =  50, p_ratio = 30,
                        show.plot=FALSE){

  origins <- list()

  flush.console <- select <- prob <- theme_bw<-
    slice <- chull <- x <- y <- ggplot <- geom_point <-
    aes <- geom_polygon <- NULL

  set.seed(1234)
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

  no_of_non_dom <- round(npoints*(100-p_ratio)/100, digits=0)
  no_of_dom <- round(npoints*(p_ratio)/100, digits = 0)

  #create labels
  #check to ensure that the total adds up
  if((no_of_non_dom + no_of_dom) < npoints){
    no_of_non_dom <- no_of_non_dom + 1
    Origins <- c(rep("Non-dominant", no_of_non_dom),
                rep("Dominant", no_of_dom))
  }

  if((no_of_non_dom + no_of_dom) == npoints){
    Origins <- c(rep("Non-dominant", no_of_non_dom),
                rep("Dominant", no_of_dom))
  }


  ran_points_prob <- data.frame(ran_points_prob,
                                Origins)

  #if(show.plot==TRUE){

    hull <- data.frame(poly) %>%
      slice(chull(x, y))

    # plot(data.frame(poly)$x, data.frame(poly)$y)
    # plot(hull$x, hull$y)

    p <- ggplot(data = ran_points_prob) +
      geom_point(mapping = aes(x = x, y = y, colour = Origins))#+

    #flush.console()
    p<-p + geom_polygon(data = hull%>%select(x,y),
                     aes(x=x, y=y), col="gray80",fill="NA",alpha = 0.9)+
      theme_bw()

  #}

  origins$origins <- ran_points_prob
  origins$plot <- p
  #Given event count at a temporal bin,
  #simulate walkers to generate the number of event
  #count

  return(origins)
}

