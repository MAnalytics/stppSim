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
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @param show.plot (TRUE or FALSE) To display plot showing
#' points (origins).
#' @usage constrained_spo(bpoly, p_ratio = 5,
#' show.plot=FALSE)
#' @examples
#' @details
#' @return Returns the global temporal pattern
#' @references
#' #https://online.stat.psu.edu/stat510/lesson/6/6.1
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @importFrom grDevices chull
#' @importFrom dplyr rename filter
#' @importFrom sf st_union st_centroid
#' @export
#'

constrained_spo <- function(bpoly, p_ratio = 5,
                          show.plot=FALSE){

  Class <- filter <- head <- ggplot <- geom_point <-
    aes <- x <- y <- geom_polygon <- hull <- theme_bw <-
    sdf <- NULL
  #San_Francisco

    origins <- list()

    #convert to simple feature
    poly2 <- st_as_sf(bpoly)

    #check if 'class' field is named incorrectly
    #check that a binary 'class' field exist
    if("Class" %in% names(poly2)){
      poly2 <- poly2 %>%
        rename(class = Class)
        flush.console()
        print(" 'Class' field is renamed as 'class'! ")
    }

    #check that a binary 'class' field exist
    if(!"class" %in% names(poly2)){
      stop(paste("There is no binary field 'class'",
                 "in the attribute of the spatialPolygonDataFrames",
                 sep = " "))
    }

    #create a boundary map from the base map
    #combine to derive boundary
    boundMap <- st_union(poly2) #plot(boundMap)

    #assign prob. values to 'enabled' origins
    #in accordance with the pareto ratio

    #generate the centroid points of 'enabled' origins
    xy <- poly2 %>%
      st_centroid() %>%
      filter(class == 1) %>%
      st_coordinates() %>%
      data.frame()

    colnames(xy) <- c("x", "y")

    #assign prob. values in accord. with pareto
    #(x/10 * sqrt(A) = x') (Courtesy: Odekadzo)
    prob_values <- p_prob(npoints=nrow(xy), p_ratio = p_ratio)

    #randomise the prob values
    set.seed(1000)
    prob_values_ <- sample(prob_values$prob, length(prob_values$prob), replace = FALSE)

    #check
    #sum(prob_values[1:round(npoints*.8, digits=0),4])
    #sum(prob_values[round(npoints*.8, digits=0):nrow(prob_values),4])

    set.seed(2000)
    xy <- xy[sample(1:nrow(xy), nrow(xy), replace = FALSE),]

    head(xy)

    #append pareto prob. values to random points
    #change to 'constr..'
    ran_points_prob <- data.frame(cbind(xy,
                         prob=prob_values_))

    no_of_non_dom <- round(nrow(xy)*(100-p_ratio)/100, digits=0)
    no_of_dom <- round(nrow(xy)*(p_ratio)/100, digits = 0)

    #create labels
    #check to ensure that the total adds up
    if((no_of_non_dom + no_of_dom) < nrow(xy)){
      no_of_non_dom <- no_of_non_dom + 1
      Origins <- c(rep("Non-dominant", no_of_non_dom),
                   rep("Dominant", no_of_dom))
    }

    if((no_of_non_dom + no_of_dom) == nrow(xy)){
      Origins <- c(rep("Non-dominant", no_of_non_dom),
                   rep("Dominant", no_of_dom))
    }


    ran_points_prob <- data.frame(ran_points_prob,
                                  Origins)

    #if(show.plot==TRUE){

    #I am not using this! cos not plotting this
    ##hull <- data.frame(poly) %>%
     ##slice(chull(x, y))

    # plot(data.frame(poly)$x, data.frame(poly)$y)
    # plot(hull$x, hull$y)

    p <- ggplot(data = ran_points_prob) +
      geom_point(mapping = aes(x = x, y = y, colour = Origins))#+

    #flush.console()#

    #plot on the basemap
    # p<-p + geom_polygon(data = hull%>%select(x,y),
    #                     aes(x=x, y=y), col="gray80",fill="NA",alpha = 0.9)+
    #   theme_bw()

    #}

    origins$origins <- ran_points_prob
    origins$plot <- p
    #Given event count at a temporal bin,
    #simulate walkers to generate the number of event
    #count

    return(origins)









    sdf

  }
