#' @title Spatial and temporal model
#' @description To generate graphics depicting the spatial
#' and temporal models of the final simulation
#' @param pt a data frame with the first three
#' fields being 'x', 'y', and 'z' information.
#' @param poly (An sf or S4 object)
#' a polygon shapefile defining the extent of a landscape.
#' Default: \code{NULL}, in which the spatial extent
#' of `pt` is utilized.
#' @param df a vector or 1-column data frame containing
#' values for the time series.
#' @param crsys (string) the EPSG code of the projection
#' system of the `ppt` coordinates. This only used if
#' `poly` argument is \code{NULL}.
#' See "http://spatialreference.org/" for the list of
#' EPSG codes for different regions of the world.
#' As an example, the EPSG code for the British National Grid
#' projection system is: \code{"EPSG:27700"}.
#' @param display_output (logical) display the output.
#' Default: \code{FALSE}
#' @usage stm(pt, poly, df, crsys = NULL,
#' display_output = FALSE)
#' @examples
#' #load polygon shapefile
#' load(file = system.file("extdata", "camden.rda",
#' package="stppSim"))
#' camden_boundary = camden$boundary
#' #read xyz data
#' data(xyz)
#' #create a time series
#' t <- seq(0,5,0.5)
#' df <- data.frame(data = abs(min(sin(t))) + sin(t))
#' #run function
#' stm(pt = xyz, poly=camden_boundary, df=df,
#' crsys = NULL, display_output = FALSE)
#' @details
#' @return Too...
#' @importFrom dplyr mutate select
#' @importFrom raster projection crop mask
#' rasterToPoints
#' @importFrom ggplot2 ggplot geom_line labs
#' geom_tile scale_fill_brewer coord_equal
#' element_blank xlab ylab ggtitle geom_sf
#' guides guide_legend
#' @importFrom sf st_crs st_as_sfc st_make_grid
#' st_drop_geometry st_centroid
#' @importFrom methods as
#' @importFrom terra interpolate
#' @importFrom cowplot plot_grid
#' @importFrom gstat gstat
#' @export


stm <- function(pt = NULL, poly = NULL, df = NULL,
                crsys = NULL, display_output = FALSE){

  x <- y <- z <- . <- cuts <-
  t_step <- data <- NULL

  if(is.null(poly)){
    stop("'poly' can not be NULL!")
  }

  if(is.null(df)){
    stop("'df' can not be NULL!")
  }

  #test polygon
  poly_tester(poly)

  pt <- data.frame(pt)
  pt <- pt[,1:3]
  colnames(pt) <- c("x", "y", "z")
  #----------------------------------
  #check if boundary is supplied,
  #if null, create an arbitrary boundary
  if(is.null(poly)){
    ppt_xy <- pt[,1:2]
    #create boundary from points
    boundary_ppt <- chull_poly(ppt_xy)
    #then define the crs
    if(is.null(crsys)){
      stop(paste("'crsys' argument cannot be NULL",
                 "when 'poly' argument is NULL!!",
                 "Needs to define the 'crsys' argument", sep=" "))
    } else {
      suppressWarnings(
        proj4string(boundary_ppt) <- CRS(crsys))
      #warning msg
    }

  } else {

    if(is.na(crs(poly))){
      stop("The 'poly' object has no projection, i.e. crs")
    }

    #first check that 'poly' has
    #a projection
    if(!is.null(crsys)){
      flush.console()
      print(paste("Note: The projection system (crs) of 'poly'",
                  "object is utilized!!", sep=" "))
    }
    if(is.na(crs(poly))){
      stop(paste("'poly' object must have a projection!"))
    }
    boundary_ppt <- poly
  }

  #spatial and temporal model
  P <- boundary_ppt
  ext_crs <- projection(P)
  grd <- make_grids(P, 150)
  suppressWarnings(
  grd <- st_centroid(st_as_sf(grd)))
  grd$x <- st_coordinates(grd)[,1]
  grd$y <- st_coordinates(grd)[,2]
  #coordinates(grd) <- c("X", "Y")
  grd_template <- grd
  #plot(grd_template)

  #join spatial origin
  org <- pt
  org_pt <- st_as_sf(org,coords = c("x", "y"),
                   crs = st_crs(grd_template))
  #origin point intersection
  #org_ptIntxn <- st_intersection(org_pt,
  #st_as_sf(grd_template))
  org_pt$x <- st_coordinates(org_pt)[,1]
  org_pt$y <- st_coordinates(org_pt)[,2]
  org_pt <- org_pt %>%
    select(x, y, z)

  #collate the extent of polygon
  coordP <- extent(P)
  sw <- cbind(coordP@xmin, coordP@ymin, 0)
  se <- cbind(coordP@xmax, coordP@ymin, 0)
  nw <- cbind(coordP@xmin, coordP@ymax, 0)
  ne <- cbind(coordP@xmax, coordP@ymax, 0)

  xtr_pt <- data.frame(rbind(sw, se, nw, ne))
  colnames(xtr_pt) <- c("x","y","z")

  xtr_pt <- st_as_sf(xtr_pt, coords = c("x", "y"),
                   crs=st_crs(grd_template))

  xtr_pt$x <- st_coordinates(xtr_pt)[,1]
  xtr_pt$y <- st_coordinates(xtr_pt)[,2]

  xtr_pt <- xtr_pt %>%
    select(x, y, z)

  #combine
  org_pt <- org_pt %>%
    bind_rows(xtr_pt)

  #grd_template <- org_pt

  # grid_plot <- ggplot() +
  #   geom_point(data = grd_template, aes(x = x, y = y), size = 0.01) +
  #   geom_point(data = org_pt,
  #            mapping = aes(x = x, y = y, color = (prob)), size = 3) +
  #  scale_color_gradientn(colors = c("blue", "yellow", "red"))+
  #   theme_bw()

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

  fit_NN <- gstat( # using package {gstat}
    formula = z ~ 1,    # The column `NH4` is what we are interested in
    data = as(sf_org_pt, "Spatial"), # using {sf} and converting to {sp}, which is expected
    nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
  )

  # Nearest Neighbor
  suppressMessages(
  interp_NN <- interpolate(grd_template_raster, fit_NN))
  #plot(interp_NN)

  ## crop and mask
  r2 <- crop(interp_NN, extent(P))
  #plot(r2)
  r3 <- mask(r2, P)

  ## Check that it worked
  #plot(r3)
  #plot(P, add=TRUE, lwd=1)

  r_points = rasterToPoints(r3)
  r_df = data.frame(r_points)
  #head(r_df) #breaks will be set to column "layer"
  r_df$cuts=cut(r_df$var1.pred,breaks=7) #set breaks

  #res$stocktype = factor(res$stocktype, levels = c("cont", "bare") )

  suppressWarnings(
  spatial_model <- ggplot(data=r_df) +
    geom_tile(aes(x=x,y=y,fill=cuts)) +
    geom_sf(data = st_as_sf(P), fill = NA) +
    # coord_sf(xlim = c(coordP@xmin, coordP@xmax),
    #          ylim = c(coordP@ymin,coordP@ymax), expand = FALSE)+
    scale_fill_brewer("Origin strength \ndistribution",type = "seq", palette = "Greys") +
    geom_point(data = org_pt %>%filter(z != 0),
               mapping = aes(x = x, y = y), shape=19, size = 2) +
    #scale_color_manual(values = c("black") ) +
    guides(color = guide_legend( override.aes = list(size = c(1.5))))+
    #coord_equal() +
    theme_bw() +
    theme(panel.grid.major = element_blank()) +
    xlab("x") + ylab("y") +
    ggtitle("Spatial model"))

  suppressWarnings(
  spatial_model  <-  spatial_model +
    geom_point(data = org_pt[1,],
               aes(x = x, y = y, size="Event \norigins", shape = NA),
               colour = "black"))

  #Temporal models
  df <- data.frame(df)
  colnames(df) <- "data"
  temporal_data <- data.frame(df) %>%
    dplyr::mutate(t_step = 1:length(df[,1]))%>%
    #dplyr::mutate(firstPeak = first_pDate)%>%
    mutate(first_peak = "twodash")

  temporal_model <- ggplot(data=temporal_data, #%>%
                                 # mutate(peak = factor(firstPeak,
                                 #                      levels = c(first_pDate)))),
                         aes(x=t_step, y=data)) +
    geom_line(aes(linetype="dash"), size=1, lty=2) +
    labs(y= "Event count", x = "time step") +
    ggtitle("Temporal model")+
    theme_bw()

  suppressWarnings(
  fn_plot <- plot_grid(spatial_model,
     temporal_model,
     ncol=1,
     rel_heights = c(3, 1),
     rel_widths = c(2,1),
     labels = c('A', 'B'),
     label_size = 12))

  #to show plot
  if(display_output == TRUE){
    flush.console()
    print(fn_plot)
  }

  return(fn_plot)

}
