#' @title Snapping point to network
#' @description Snaps points to the nearest segment
#' of a network data.
#' @param points point data (sf object)
#' @param lines line/street/road network (sf object)
#' @param verbose Whether to output processing
#' messages.
#' @usage snap_points_to_lines(points, lines,
#' verbose = FALSE)
#' @examples
#'
#' #get line and point data
#' #load(file = system.file("extdata", "camden.rda",
#' #package="stppSim"))
#' lines <- stppSim:::lines
#' pts <- stppSim:::pts
#' my_points <- snap_points_to_lines(points=pts,
#' lines=lines,
#' verbose = FALSE)
#'
#' #preview result
#' #ggplot()+
#' #geom_sf(data = lines, col = 'red')+
#' #geom_sf(data = pts, shape = 1)

#' @details Function snaps points (within 300m)
#' to the nearest segment on a network. The remaining
#' points outside 300m buffer are returned in
#' their original locations (Credit: Michal Kvasnicka)
#' @return Point (sf object) with adjusted coordinates
#' to fit on the network data
#' @importFrom sf st_union st_buffer st_intersects
#' st_nearest_feature st_nearest_points st_cast
#' st_drop_geometry st_as_sf
#' @importFrom dplyr bind_rows
#' @export
#'

snap_points_to_lines <- function(points, lines,
                                 verbose = FALSE){

  all_points = TRUE
  dist = 300

  lines$idunique <- 1:nrow(lines)

  buffer_lines <- function(lines, dist = 300, verbose = FALSE) {
    if (verbose) message("Creating buffer for lines...")
    st_buffer(lines, dist = dist) |> st_union()
  }

  get_points_close_to_lines <- function(points, lines, dist = 300,
                                        verbose = FALSE) {
    buff <- buffer_lines(lines, dist = dist, verbose = verbose)
    if (verbose) message("Finding which points are in buffer...")
    st_intersects(points, buff, sparse = FALSE) #%>%
      #lapply(., function(n){lines$idunique[n]})
  }


  ##snap_points_to_lines <- function(points, lines, dist = 300,
                                   ##all_points = FALSE, verbose = FALSE) {

    inside <- get_points_close_to_lines(points, lines, dist = dist,
                                        verbose = verbose)
    buff_points <- points[inside, ]
    if (verbose) message("Finding nearest features...")
    nf <- st_nearest_feature(buff_points, lines)
    if (verbose) message("Finding nearest points...")
    np <- st_nearest_points(buff_points, lines[nf, ], pairwise = TRUE)
    np <- st_cast(np, "POINT")[c(FALSE, TRUE)]
    if (verbose) message("Adding attributes...")
    out <- st_drop_geometry(buff_points)
    out$geometry <- np
    out <- st_as_sf(out)
    if (all_points) {
      if (verbose) message("Adding points outside buffer...")
      out$valid <- TRUE
      if(length(which(out$valid=="TRUE"))!=nrow(points)){
      outside <- points[!inside, ]
      outside$valid <- FALSE
      out <- dplyr::bind_rows(out, outside)
      }
      if(length(which(out$valid=="TRUE"))==nrow(points)){
      out <- out
      }
    }
    return(out)
  ##}

}






