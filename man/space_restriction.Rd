% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/space_restriction.R
\name{space_restriction}
\alias{space_restriction}
\title{Space restriction map (raster)}
\usage{
space_restriction(shp, baseMap, res, binary = is.na(field),
field = NA, mapvalues = NA, background = 1)
}
\arguments{
\item{shp}{polygon shapefile object.}

\item{baseMap}{if provided, a raster onto which
to stack the next rasterized shapefile.}

\item{res}{the desired pixel resolution of the
raster to be created, when baseMap is not provided.}

\item{binary}{if TRUE, the shapefile will be rasterized
so that any feature is assigned a value of 0 (least resistance
to event occurences),
and the background 1 (most resistance to event occurences).}

\item{field}{(string) the name of the character
field from which to extract the label of different
feature classes.}

\item{mapvalues}{a named vector specifying the resistance
value mapping from the classes of field.}

\item{background}{the value in the range 0 and 1
to assign to all pixels that are not covered
by any shapefile feature.}
}
\value{
Returns a raster map
}
\description{
Build a space restriction map
from one or more shapefiles. A space restriction map
(raster) shows the restrictions (to event occurences)
across space. Function builds
on raster- and SimRIv-packages.
}
\details{
Help to create a complete space restriction map
with cell values ranging from 0(lowest) and 1(highest). All
other area not covered by any features is assigned the value
of \code{background}. When stacking additional features to existing
\code{baseMap}, only the areas covered by features are updated, while
the remaining areas retain the original values of \code{baseMap}.
}
\examples{
data(camden_boundary)
restrct_space <- space_restriction(shp = camden_boundary,
res = 100, binary = TRUE)
#plot the result
#plot(restrct_space)
#Setting 'restrct_space' raster as basemap, the landuse
#map can now be stacked onto the basemap as follows:
data(camden_landuse)
restrct_Landuse <- space_restriction(shp = camden_landuse,
baseMap = restrct_space,
res = 100, field = "type", mapvalues = c("Green" = 0.8,
"Leisure" = 1, "Sports" = 0.7), background = 1)
#plot(restrct_Landuse)
}
\references{
\enumerate{
\item Paul Murrell (2019). rasterize: Rasterize Graphical
Output. R package version 0.1.
https://CRAN.R-project.org/package=rasterize
\item Quaglietta L, Porto M (2019). \emph{SiMRiv: Individual-Based,
Spatially-Explicit Simulation and Analysis of Multi-State
Movements in River Networks and Heterogeneous Landscapes}.
R package version 1.0.4, <URL:
https://CRAN.R-project.org/package=SiMRiv>.
}
}