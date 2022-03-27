#' @title Compare two areas
#' @description To compare the sizes of two
#' areas (boundary shapefiles).
#' @param area1 (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). the polygon object of the
#' first area.
#' @param area2 (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). the polygon object of the
#' second area.
#' @param display_output (logical) Whether to show output - a plot
#' comparing the two areas (plotted at the same scale), and a
#' statement stating the comparison.
#' Default: \code{FALSE}
#' @usage compare_areas(area1, area2,
#' display_output = FALSE)
#' @examples
#' #load 'area1' object - boundary of Camden, UK
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' camden_boundary = camden$boundary
#'
#' #load 'area2' - boundary of Birmingham, UK
#' load(file = system.file("extdata", "birmingham_boundary.rda",
#' package="stppSim"))
#'
#' #run
#' compare_areas(area1 = camden_boundary,
#' area2 = birmingham_boundary, display_output = FALSE)
#' @details Compares the sizes of two areas (polygon shapefiles).
#' The two shapefiles can be in any `crs`,
#' and any spatial object formats. If enabled, the output
#' is shown comprising a plot and a string quoting the
#' factor by which
#' one area is larger than the other. This factor can
#' be used to scale specific spatial parameters, such as
#' `n_orign`, `s_threshold`, and `step_length`, during simulation
#' for a new study area.
#' @return Returns a plot and a text (string) comparing
#' the sizes of two areas.
#' @importFrom sf st_as_sf st_transform st_bbox
#' as_Spatial
#' @importFrom dplyr mutate select
#' @importFrom geosphere distHaversine areaPolygon
#' @importFrom purrr map
#' @importFrom cowplot plot_grid
#' @importFrom ggsn scalebar
#' @importFrom ggplot2 ggplot ggtitle
#' geom_sf
#' @export


compare_areas <- function(area1, area2, display_output = FALSE){

  st_transform <- st_bbox <- area <- geometry <-
    distm <- distHaversine <- areaPolygon <- map <-
    xlab <- ylab <- scalebar <- plot_grid <- g1 <- NULL

  output <- list()

  #converting to wgs84
  area1_wgs <- st_as_sf(area1)
  area1_wgs <- st_transform(area1_wgs, crs=4326)
  extent1 <- st_bbox(area1_wgs)

  area1_wgs <- area1_wgs %>%
    mutate(area = "area1") %>%
    dplyr::select(area, geometry)

  area2_wgs <- st_as_sf(area2)
  area2_wgs <- st_transform(area2_wgs, crs=4326)
  extent2 <- st_bbox(area2_wgs)

  area2_wgs <- area2_wgs %>%
    mutate(area = "area2") %>%
    dplyr::select(area, geometry)

  #combine maps
  combine_map <- rbind(area1_wgs, area2_wgs)
  #plot(combine_map)

  #convert extent to km
  #dist1 <- distm(c(extent1[1], extent1[2]), c(extent1[3], extent1[4]), fun = "distHaversine")
  #dist2 <- distm(c(extent2[1], extent2[2]), c(extent2[3], extent2[4]), fun = "distHaversine")

  dist1 <- distHaversine(c(extent1[1], extent1[2]), c(extent1[3], extent1[4]))
  dist2 <- distHaversine(c(extent2[1], extent2[2]), c(extent2[3], extent2[4]))

  #area of polygons
  area1_area <- areaPolygon(as_Spatial(area1_wgs))
  area2_area <- areaPolygon(as_Spatial(area2_wgs))

  if(area1_area > area2_area) {
    out <- paste("#-----'area1' is", round(area1_area/area2_area, digits = 1),
               "times larger than 'area2'-----#", sep=" ")
    val <- round(area1_area/area2_area, digits = 1)
    }

  if(area2_area > area1_area) {
    out <- paste("#-----'area2' is", round(area2_area/area1_area, digits = 1),
               "times larger than 'area1'-----#", sep=" ")
    val <- round(area2_area/area1_area, digits = 1)
    }

  g1 <- purrr::map(unique(combine_map$area),
            function(x) {
              # subset data
              temp_sf <- subset(combine_map, area == x)
              ggplot() +
                geom_sf(data = temp_sf, fill='white') +
                #guides(fill = "none") +
                ggtitle(x) +
                scalebar(temp_sf, dist = val, st.size=2,
                         height=0.01, model = 'WGS84',
                         transform = T, dist_unit='km')
              })

  g1 <- cowplot::plot_grid(plotlist = g1)

  if(display_output == TRUE){
    flush.console()
    print(g1)
    flush.console()
    print(out)
  }

  output$comparison <- out
  output$plot <- g1

  return(output)

}
