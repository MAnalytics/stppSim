#' @title Artificial (random) spatial point origins
#' @description Simulate artificial point origin across a
#' defined spatial boundary. If resistance vector map
#' is supplied, the space occupied by the vector map is
#' ignored. In other words, no point origin is simulated inside
#' the resistance space. Each origin is assigned
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
#' @param space_resist ("SpatialPolygonsDataFrame",
#' "SpatialPolygons", or "sf") Optional features showing
#' spaces across landscape within which spatial
#' points (origins) are not allowed. Default: \code{NULL}.
#' @param  n_foci (an integer) A value indicating the number of
#' focal points amongst event origins.
#' @param foci_separation (an integer) A value between `0` and `10`
#' indicating the nearness of focal points from one another. A `0`
#' separation indicates all focal points located in a close proximity
#' while `10` indicates focal points that are evenly distributed across
#' space.
#' @param p_ratio (an integer) The smaller of the
#' two terms of the Pareto ratio. For example, for a \code{20:80}
#' ratio, `p_ratio` will be \code{20}. Default value is
#' \code{30}. Valid inputs are \code{10}, \code{20},
#' \code{30}, \code{40}, and \code{50}. A \code{30:70}, represents
#' 30% dominant and 70% non-dominant origins.
#' @usage artif_spo(poly, n_origin, space_resist, n_foci=5,
#' foci_separation = 0, p_ratio)
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
#' @importFrom stats dist kmeans
#' @export
#'


artif_spo <- function(poly, n_origin =  52, space_resist = NULL,
                      n_foci=5,
                      foci_separation = 0, p_ratio = 30){

  #define global variables
  data_frame <- dist <- kmeans <-
    if_else <- row_number <- category <- NULL


  #check the inputs
  if(n_origin <= 0){
    stop("Number of origin points need to be specified!")
  }

  #check values of focal point and
  #foci separations
  if(n_foci >= n_origin){
    stop("focal point cannot be greater than the number of origins!")
  }

  if(!foci_separation %in% 0:10){
    stop("Foci separation should be an integer value between 0 and 10")
  }

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
    #generate random points inside the boundary
    ran_points <- as.data.frame(csr(as.matrix(poly,,2), n_origin))
    colnames(ran_points) <- c("x", "y")
    #plot(ran_points$x,ran_points$y)

    final_ran_points_pt <- ran_points

  if(!is.null(space_resist)){

    #loop through
    camden_landuse <- st_as_sf(camden_landuse)
    #convert xy to points
    ran_points_pt <- st_as_sf(SpatialPoints(cbind(ran_points$x, ran_points$y),
                                proj4string = crs(camden_landuse)))
    #check those intersecting land use
    pt_intersect <- unique(data.frame(st_intersects(ran_points_pt, camden_landuse))[,1])
    new_ran_points_pt <- ran_points_pt[-pt_intersect,]
    #---------------------------
    #check if any of the point intersects
    #resistance feature across the space.
    final_ran_points_pt <- new_ran_points_pt

    #loop until there is exactly number
    #of specified origin points
    while(nrow(final_ran_points_pt) < n_origin){

      #simulate another set of points
      #generate random points inside the boundary
      ran_points <- as.data.frame(csr(as.matrix(poly,,2), n_origin))
      colnames(ran_points) <- c("x", "y")
      #convert to points and check intersection
      ran_points_pt <- st_as_sf(SpatialPoints(cbind(ran_points$x, ran_points$y),
                                              proj4string = crs(camden_landuse)))
      #check those not intersecting land use
      pt_intersect <- unique(data.frame(st_intersects(ran_points_pt, camden_landuse))[,1])
      new_ran_points_pt <- ran_points_pt[-pt_intersect,]

      #add to existing list
      final_ran_points_pt <- rbind(final_ran_points_pt, new_ran_points_pt)
    }

    #check if the number of point is greater
    #than specified number
    if(nrow(final_ran_points_pt) > n_origin){
      final_ran_points_pt <- final_ran_points_pt[1:(nrow(final_ran_points_pt) -
                                          ((nrow(final_ran_points_pt) - n_origin))),]
    }
  }
  #---------------------------

  ##plot(as_Spatial(final_ran_points_pt), add=TRUE)
  ##plot(as_Spatial(camden_landuse))


  #--------------
  #utilize 'n_foci' - number of foci
  #'foci_separation' - distances of foci points
  #'from one another. levels: 1(low) - 10(high)
  #' to assign
  #probability values based on the specified pareto ratio
  #--------------
  #randomly select 'n_foci' points, based on foci
  #proximities

  #add x, y coordinates
  final_ran_points_pt$x <- st_coordinates(final_ran_points_pt)[,1]
  final_ran_points_pt$y <- st_coordinates(final_ran_points_pt)[,2]

  final_ran_points_pt <- final_ran_points_pt %>%
    data_frame() %>%
    dplyr::select(c(x, y))

  o_dist <- dist(final_ran_points_pt, method = "euclidean", upper=TRUE, diag = TRUE)

  #randomly pick one point as the
  #main focus
  set.seed(1000)
  idx <- sample(1:nrow(final_ran_points_pt), 1, replace=FALSE)
  #now sort the dist matrix from selected points
  dist_to_main_focus <- as.matrix(o_dist)[,idx]
  #order of proximity
  dist_to_main_focus <- dist_to_main_focus[order(dist_to_main_focus)]#[2:length(dist_to_main_focus)]
  idx_others <- names(dist_to_main_focus)#[2:length(names(dist_to_main_focus))]

  #get the foci_separation and determine  #n_foci=5; foci_separation <- 0
  #where the n_foci points fall
  #then use pareto ratio to assign prob points,
  #looping through foci.

  separation_list <- data.frame(cbind(sn=0:10, val=10:0))

  list_to_pick_from <- length(dist_to_main_focus) -
    (floor(length(dist_to_main_focus)/11)*separation_list[which(separation_list$sn == foci_separation),2])
  #10 is the max. foci separation
  #list_to_pick_from

  #then pick random 'n_foci' from the 'list_to_p....'
  set.seed(2000)
  n_foci_centre <- sample(idx_others[1:list_to_pick_from], n_foci, replace =FALSE)

  #set as kmean centroids
  #group with 1 iteration
  groups <- kmeans(final_ran_points_pt, final_ran_points_pt[as.numeric(n_foci_centre),], iter.max = 1, nstart = 1,
         algorithm = "Lloyd", trace=FALSE)

  #now collate members of each group
  #assign probablity value

  groups_clusters <- data.frame(cbind(final_ran_points_pt, group=groups$cluster))

  #append origin category
  groups_clusters <- groups_clusters %>%
    mutate(category = if_else(row_number() %in% as.numeric(n_foci_centre),
                              paste("focal_pt"), paste("others")))



  #now sort in order of proximity to
  #main focal point
  groups_clusters <- groups_clusters[as.numeric(idx_others),]

  #move the main 'focal_pt' to the top
  #main focal pts
  groups_clusters_focal <- groups_clusters %>%
    filter(category == "focal_pt")
  #others
  groups_clusters_others <- groups_clusters %>%
    filter(category == "others")

  groups_clusters <-
    rbind(groups_clusters_focal, groups_clusters_others)

  grp_p <- p_prob(n_origin=nrow(groups_clusters), p_ratio = p_ratio)

  #append pareto prob. values to random points
  final_ran_points_pt_prob <-
    cbind(groups_clusters, prob=rev(grp_p$prob))


  #if(show.plot==TRUE){

    hull <- data.frame(poly) %>%
      slice(chull(x, y))

    # plot(data.frame(poly)$x, data.frame(poly)$y)
    # plot(hull$x, hull$y)

    p <- ggplot(data = final_ran_points_pt_prob) +
      geom_point(mapping = aes(x = x, y = y, color = category))#+


    # if(show.plot==TRUE){
    #   flush.console()
    #   p + geom_polygon(data = hull%>%dplyr::select(x,y),
    #                  aes(x=x, y=y), col="gray80",fill="NA",alpha = 0.9) +
    #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    #     theme_light()
    #  }

  #}

  origins$origins <- final_ran_points_pt_prob
  origins$plot <- p
  origins$poly <- backup_poly
  origins$Class <- "artif_spo"
  #Given event count at a temporal bin,
  #simulate walkers to generate the number of event
  #count

  return(origins)
}

