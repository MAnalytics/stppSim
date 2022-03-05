#' @title Artificial spatial event origins
#' @description Simulate spatial points to serve as event
#' origins across the space. If provided, resistance features
#' are to be avoided. Each final
#' event origin point is assigned a probability value
#' indicating the strength of the origin.
#' @param poly (An sf or S4 object)
#' A polygon shapefile within which
#' event origins are to be situated.
#' @param n_origin (an integer) Value specifying
#' the number of event origins to synthetize.
#' Default:\code{50}. Value specified here has the greatest
#' influence on the computational time.
#' @param resistance_feat (An S4 object) Optional
#' shapefile representing spaces across landscape
#' within which event
#' origins are not allowed. Default: \code{NULL}.
#' @param  n_foci (an integer) Value indicating the number of
#' focal points amongst event origins. `n_foci` will usually be
#' smaller than `n_origin`.
#' @param foci_separation (an integer) A percentage value indicating
#' indicating the nearness of focal points from one another. A `0`
#' separation indicates that focal points are in close proximity
#' of one another, while a `100` indicates focal points being
#' evenly distributed across space.
#' @param conc_type (string) Specifies the spatial pattern
#' of non-focal origin (strengths) in relation to
#' to their nearest focal origins. Value is either
#' \code{"nucleated"} or \code{"dispersed"}.
#' @param p_ratio (an integer) The smaller of the
#' two terms of a Pareto ratio.
#' For example, a value of \code{20}
#' implies a \code{20:80} Pareto ratio.
#' @usage artif_spo(poly, n_origin=50, resistance_feat = NULL,
#' n_foci=5, foci_separation = 10,
#' conc_type = "nucleated", p_ratio)
#' @examples
#' data(camden_boundary)
#' data(landuse)
#' spo <- artif_spo(poly = camden_boundary, n_origin = 50,
#' resistance_feat = landuse, n_foci=5,
#' foci_separation = 0, conc_type = "dispersed", p_ratio=20)
#' @details Details of events origins:
#' {x,y locations, categories (i.e. focal and non-focal (others)
#' origins), and the probability values.
#' and non-focal point}.
#' The focal origins (`n_foci`) serve as the more dominant
#' origins (e.g. city centres), while the non-focal origins
#' (i.e. non-dominant) origin. The `foci_separation` indicates
#' the nearness of dominant origins from one another.
#' The `conc_type` argument allows a user to specify
#' the type of spatial patterns exhibited by the non-focal
#' points around the focal points (See vignette for details).
#' If `resistance_feat` is provided, the features help
#' to prevent event origins from being
#' situated in the same locations occupied by the features.
#' @return Returns event origins with their respective
#' strength (probability) values.
#' @importFrom dplyr if_else mutate filter
#' row_number select bind_cols
#' @importFrom splancs csr
#' @importFrom utils flush.console
#' @importFrom grDevices chull
#' @importFrom ggplot2 ggplot geom_point
#' geom_polygon theme_bw
#' @importFrom stats dist kmeans
#' @export

artif_spo <- function(poly, n_origin =  50, resistance_feat = NULL,
                      n_foci=5, foci_separation = 10,
                      conc_type = "nucleated", p_ratio = 20){

  origins <- list()

  #define global variables
  group <- bind_cols <- data_frame <- dist <- kmeans <-
    if_else <- row_number <- category <-
    flush.console <- as <- select <- prob <-
    theme <- theme_bw <-
    theme_light <- element_text <-
    slice <- chull <- x <- y <- ggplot <- geom_point <-
    aes <- geom_polygon <- NULL


  #check the inputs
  if(n_origin <= 0){
    stop("Number of origin points need to be specified!")
  }

  #check values of focal point and
  #foci separations
  if(n_foci >= n_origin){
    stop("focal point cannot be greater than the number of origins!")
  }

  #convert percentage to numeric
  perc_sep <- as.numeric(sub("%","",foci_separation))
  perc_sep <- round(perc_sep, digits = 0)

  if(!perc_sep %in% 0:100){
    stop("Foci separation should be a value between 1 and 100!")
  }

  poly_tester(poly)

  #backup
  backup_poly <- poly

  poly <- extract_coords(poly)

    #set.seed(1234)
    #generate random points inside the boundary
    ran_points <- as.data.frame(csr(as.matrix(poly,,2), n_origin))
    colnames(ran_points) <- c("x", "y")
    #plot(ran_points$x,ran_points$y)



  if(is.null(resistance_feat)){

    ran_points_pt <- st_as_sf(SpatialPoints(cbind(ran_points$x, ran_points$y),
                           proj4string = crs(backup_poly)))

    final_ran_points_pt <- ran_points_pt
    }

  if(!is.null(resistance_feat)){

    #loop through
    resistance_feat <- st_as_sf(resistance_feat)
    #convert xy to points
    ran_points_pt <- st_as_sf(SpatialPoints(cbind(ran_points$x, ran_points$y),
                                proj4string = crs(resistance_feat)))
    #check those intersecting land use
    pt_intersect <- unique(data.frame(st_intersects(ran_points_pt, resistance_feat))[,1])
    new_ran_points_pt <- ran_points_pt[-pt_intersect,]

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
                                              proj4string = crs(resistance_feat)))
      #check those not intersecting land use
      pt_intersect <- unique(data.frame(st_intersects(ran_points_pt, resistance_feat))[,1])
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


  final_ran_points_pt$x <- st_coordinates(final_ran_points_pt)[,1]
  final_ran_points_pt$y <- st_coordinates(final_ran_points_pt)[,2]

  final_ran_points_pt <- final_ran_points_pt %>%
    as.data.frame() %>%
    dplyr::select(c(x, y))

  #calculate distances between points
  o_dist <- dist(final_ran_points_pt, method = "euclidean", upper=TRUE, diag = TRUE)

  #randomly pick one point as the
  #main focal point
  ##set.seed(1000)
  idx <- sample(1:nrow(final_ran_points_pt), 1, replace=FALSE)
  #now sort the dist matrix from selected points
  dist_to_main_focus <- as.matrix(o_dist)[,idx]
  #order of proximity
  dist_to_main_focus <- dist_to_main_focus[order(dist_to_main_focus)]
  idx_others <- names(dist_to_main_focus)

  separation_list <- data.frame(cbind(sn=0:10, val=10:0))

  list_to_pick_from <- length(dist_to_main_focus) -
    (floor(length(dist_to_main_focus)/11)*separation_list[which(separation_list$sn ==
                             round(perc_sep/10, digits=0)),2])
  #then pick random 'n_foci' from the 'list_to_p....'
  ##set.seed(2000)
  n_foci_centre <- sample(idx_others[1:list_to_pick_from], n_foci, replace =FALSE)

  #set as kmean centroids
  #group with 1 iteration
  groups <- kmeans(final_ran_points_pt,
                   final_ran_points_pt[as.numeric(n_foci_centre),],
                   iter.max = 1, nstart = 1,
         algorithm = "Lloyd", trace=FALSE)

  #now collate members of each group
  #assign probablity value
  groups_clusters <- data.frame(cbind(final_ran_points_pt, group=groups$cluster))

  #append origin category
  groups_clusters <- groups_clusters %>%
    mutate(category = if_else(row_number() %in% as.numeric(n_foci_centre),
                              paste("focal_pt"), paste("others")))

  #if concentration type is "dispersed"
  if(conc_type == "dispersed"){
    #pick each of the focal point
    #and sort their respective 'others'
    #according to the proximity
    group_combined <- NULL

    for(z in 1:length(unique(groups_clusters$group))){ #z<-1

      gr_cut <- groups_clusters %>%
        filter(group == z)

      gr_cut_bk <- gr_cut %>%
        as.data.frame() %>%
        arrange(category)

      #sort to bring the foca_pt up
      gr_cut <- gr_cut %>%
        as.data.frame() %>%
        arrange(category)%>%
        dplyr::select(x, y)
      #dist
      o_dist_gr <- dist(gr_cut, method = "euclidean", upper=TRUE, diag = TRUE)
      o_dist_gr <- as.matrix(o_dist_gr)[1,]

      o_dist_gr <- o_dist_gr[order(o_dist_gr)]

      #in case of 1 row
      if(length(o_dist_gr)==1){
        names(o_dist_gr) <- 1
      }

      #sort the main data
      gr_cut_bk <- cbind(gr_cut_bk[as.numeric(names(o_dist_gr)),],
                         idx=1:nrow(gr_cut_bk))

      group_combined <- rbind(group_combined, gr_cut_bk)
    }

    #create prob
    grp_p <- p_prob(n=nrow(group_combined),
                    p_ratio = p_ratio)
    prob <- rev(grp_p$prob)

    #sort
    final_ran_points_pt_prob <- group_combined %>%
      arrange(idx) %>%
      bind_cols(prob=prob) %>%
      dplyr::select(-c(idx))

  }

  #if concentration type is "nucleated"
  if(conc_type == "nucleated"){
    #Sort in order of proximity to
    #main focal point
    groups_clusters <- groups_clusters[as.numeric(idx_others),]

    #move the main 'focal_pts' to the top
    groups_clusters_focal <- groups_clusters %>%
      filter(category == "focal_pt")
    #others
    groups_clusters_others <- groups_clusters %>%
      filter(category == "others")

    groups_clusters <-
      rbind(groups_clusters_focal, groups_clusters_others)

    grp_p <- p_prob(n=nrow(groups_clusters), p_ratio = p_ratio)

    #append prob. values to random points
    final_ran_points_pt_prob <-
      cbind(groups_clusters, prob=rev(grp_p$prob))
  }

  p <- ggplot(data = final_ran_points_pt_prob) +
    geom_point(mapping = aes(x = x, y = y, color = category))#+

  origins$origins <- final_ran_points_pt_prob
  origins$plot <- p
  origins$poly <- backup_poly
  origins$Class <- "artif_spo"

  return(origins)

}





