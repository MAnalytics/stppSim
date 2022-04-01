#' @title Spatial and temporal model
#' @description To generate graphics depicting the spatial
#' and temporal models of the final simulation
#' @param poly (as `spatialPolygons`,
#' `spatialPolygonDataFrames`, or
#' `simple features`). the polygon object of the
#' second area.
#' @param display_output (logical) Whether to print output
#' in the console.
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
#' and any spatial object formats. If enabled, the output (a value)
#' comparing the area of the two polygons is printed. This value can
#' be used to scale some specific spatial parameters, including
#' `n_origin`, `s_threshold`, and `step_length`.
#' @return Returns a plot and a text (string) comparing
#' the sizes of two areas.
#' @importFrom sf st_as_sf st_transform st_bbox
#' as_Spatial
#' @importFrom dplyr mutate select
#' @importFrom geosphere distHaversine areaPolygon
#' @export


stm <- function(poly = NULL, df = NULL){


#spatial and temporal model
P <- spo$poly
ext_crs <- projection(P)
grd <- make_grids(P, 250)
grd <- st_centroid(st_as_sf(grd))
grd$x <- st_coordinates(grd)[,1]
grd$y <- st_coordinates(grd)[,2]
#coordinates(grd) <- c("X", "Y")
grd_template <- grd
#plot(grd_template)

#join spatial origin
org <- spo$origins
org_pt <- st_as_sf(org,coords = c("x", "y"),
                   crs = st_crs(grd_template))
#origin point intersection
#org_ptIntxn <- st_intersection(org_pt,
#st_as_sf(grd_template))
org_pt$x <- st_coordinates(org_pt)[,1]
org_pt$y <- st_coordinates(org_pt)[,2]
org_pt <- org_pt %>%
  select(x, y, prob)

#collate the extent of polygon
coordP <- extent(P)
sw <- cbind(coordP@xmin, coordP@ymin, 0)
se <- cbind(coordP@xmax, coordP@ymin, 0)
nw <- cbind(coordP@xmin, coordP@ymax, 0)
ne <- cbind(coordP@xmax, coordP@ymax, 0)

xtr_pt <- data.frame(rbind(sw, se, nw, ne))
colnames(xtr_pt) <- c("x","y","prob")

xtr_pt <- st_as_sf(xtr_pt, coords = c("x", "y"),
                   crs=st_crs(grd_template))

xtr_pt$x <- st_coordinates(xtr_pt)[,1]
xtr_pt$y <- st_coordinates(xtr_pt)[,2]

xtr_pt <- xtr_pt %>%
  select(x, y, prob)

#combine
org_pt <- org_pt %>%
  bind_rows(xtr_pt)



#grd_template <- org_pt

grid_plot <- ggplot() +
  geom_point(data = grd_template, aes(x = x, y = y), size = 0.01) +
  geom_point(data = org_pt,
             mapping = aes(x = x, y = y, color = (prob)), size = 3) +
  scale_color_gradientn(colors = c("blue", "yellow", "red"))+
  theme_bw()

sf_org_pt <- st_as_sf(org_pt, coords = c("x", "y"))

grd_template_sf <- sf_org_pt %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(
    cellsize = c(20, 20),
    what = "centers"
  ) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(Z = 0)

grd_template_raster <- grd_template_sf %>%
  raster::rasterFromXYZ(
    crs = ext_crs
  )

fit_NN <- gstat::gstat( # using package {gstat}
  formula = prob ~ 1,    # The column `NH4` is what we are interested in
  data = as(sf_NH4, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)

# Nearest Neighbor
interp_NN <- interpolate(grd_template_raster, fit_NN)
#plot(interp_NN)

## crop and mask
r2 <- crop(interp_NN, extent(P))
#plot(r2)
r3 <- mask(r2, P)

## Check that it worked
plot(r3)
plot(P, add=TRUE, lwd=1)


r_points = rasterToPoints(r3)
r_df = data.frame(r_points)
head(r_df) #breaks will be set to column "layer"
r_df$cuts=cut(r_df$var1.pred,breaks=7) #set breaks

spatial_model <- ggplot(data=r_df) +
  geom_tile(aes(x=x,y=y,fill=cuts)) +
  scale_fill_brewer("Legend",type = "seq", palette = "Greys") +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  xlab("x") + ylab("y") +
  ggtitle("Spatial model")


#Temporal models
temporal_data <- data.frame(gtp) %>%
  dplyr::mutate(t_step = 1:length(gtp$data))%>%
  dplyr::mutate(firstPeak = first_pDate)%>%
  mutate(first_peak = "twodash")

temporal_model <- ggplot(data=(temporal_data %>%
                                 mutate(peak = factor(firstPeak,
                                                      levels = c(first_pDate)))),
                         aes(x=t_step, y=data)) +
  geom_line(aes(linetype="dash"), size=1, lty=2) +
  labs(y= "Event count", x = "time step") +
  ggtitle("Temporal_model")+
  theme_bw()

temporal_model

plot_grid(spatial_model,
          temporal_model,
          ncol=1,
          rel_heights = c(3, 1),
          rel_widths = c(2,1),
          labels = c('A', 'B'),
          label_size = 12)

}
