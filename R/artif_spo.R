#' @title Generate artificial (random) spatial point origins
#' @description Simulate artificial point origin across a
#' defined spatial boundary.Each origin is assigned
#' a probability value (representing the relative intensity) at which
#' the origin generates events with respect to a specified
#' Pareto ratio.
#' @param poly (A dataframe or S4 object) A dataframe of X, Y
#' coordinates or a spatial boundary (as "SpatialPolygonsDataFrame",
#' "SpatialPolygons", or "sf") representing the boundary within which
#' events are to be generated.
#' @param n_origin (an integer) Number of origins to simulate.
#' Default:\code{50}. This is the parameter that has the greatest
#' influence on the computational time.
#' @param p_ratio (an integer) The smaller of the
#' two terms of the Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' origins.
#' @usage random_spo(poly, n_origin, p_ratio, show.plot=FALSE)
#' @examples
#' @details Returns an object of the class `artif_spo`,
#' detailing the properties of artificial (simulated) spatial point
#' origins.
#' @return Returns random event origins
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @importFrom grDevices chull
#' @importFrom ggplot2 ggplot geom_point
#' geom_polygon theme_bw
#' @export
#'

artif_spo <- function(poly, n_origin =  50, p_ratio = 30, show.plot = FALSE){

  flush.console <- as <- select <- prob <- theme <- theme_bw <-
    theme_light <- element_text <-
    slice <- chull <- x <- y <- ggplot <- geom_point <-
    aes <- geom_polygon <- NULL

  #-----
  poly_tester(poly)
  #-----

  #backup
  backup_poly <- poly

  poly <- extract_coords(poly)

  origins <- list()



  set.seed(1234)
  #generate random pints inside the boundary
  ran_points <- as.data.frame(csr(as.matrix(poly,,2), n_origin))
  colnames(ran_points) <- c("x", "y")
  #points(ran_points$x,ran_points$y)

  #assign prob. values in accord. with pareto
  #(x/10 * sqrt(A) = x') (Courtesy: Odekadzo)
  prob_values <- p_prob(n_origin=n_origin, p_ratio = p_ratio)
  #check
  #sum(prob_values[1:round(n_origin*.8, digits=0),4])
  #sum(prob_values[round(n_origin*.8, digits=0):nrow(prob_values),4])

  #append pareto prob. values to random points
  ran_points_prob <- data.frame(cbind(ran_points,
                          prob=prob_values%>%select(prob)))

  no_of_non_dom <- round(n_origin*(100-p_ratio)/100, digits=0)
  no_of_dom <- round(n_origin*(p_ratio)/100, digits = 0)

  #create labels
  #check to ensure that the total adds up
  if((no_of_non_dom + no_of_dom) < n_origin){
    no_of_non_dom <- no_of_non_dom + 1
    OriginType <- c(rep("Non-dominant", no_of_non_dom),
                rep("Dominant", no_of_dom))
  }

  if(((no_of_non_dom + no_of_dom) != n_origin)&((no_of_non_dom + no_of_dom) > n_origin)){
    stop("Process terminated! Increase the value of 'n_origin'!")
  }

  if((no_of_non_dom + no_of_dom) == n_origin){
    OriginType <- c(rep("Non-dominant", no_of_non_dom),
                    rep("Dominant", no_of_dom))
  }

  ran_points_prob <- data.frame(ran_points_prob,
                                OriginType)

  #if(show.plot==TRUE){

    hull <- data.frame(poly) %>%
      slice(chull(x, y))

    # plot(data.frame(poly)$x, data.frame(poly)$y)
    # plot(hull$x, hull$y)

    p <- ggplot(data = ran_points_prob) +
      geom_point(mapping = aes(x = x, y = y, colour = OriginType))#+


    if(show.plot==TRUE){
      flush.console()
      p + geom_polygon(data = hull%>%select(x,y),
                     aes(x=x, y=y), col="gray80",fill="NA",alpha = 0.9) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme_light()
     }

  #}

  origins$origins <- ran_points_prob
  origins$plot <- p
  origins$poly <- backup_poly
  origins$Class <- "artif_spo"
  #Given event count at a temporal bin,
  #simulate walkers to generate the number of event
  #count

  return(origins)
}

