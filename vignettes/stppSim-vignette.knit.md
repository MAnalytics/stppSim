---
title: "An R package for synthesizing spatiotemporal point patterns for social and life science research - A user guide"

author: |
  | Monsuru Adepeju^[Big Data Centre, Manchester Metropolitan University, Manchester, M156BH, UK, m.adepeju@mmu.ac.uk]
  
date: |
  | `Date:`
  | `2022-03-20`

#output:
  #pdf_vignette
  
output:
  pdf_document

urlcolor: blue

toc:
  true

number_sections: 
  yes
    
linestretch: 
  1.5
  
fontsize: 
  16pt

#header-includes:
 # - \usepackage{leading}
  #- \leading{18}
  
#dev: png
#output:
  #word_document: default
  #always_allow_html: yes
#  pdf_document: default
always_allow_html: yes
#fig_caption: yes
bibliography: references.bib

abstract: With increasingly limited availability of fine-grained spatially and 
  temporally stamped point data, the `stppSim` provides an alternative 
  source of data for a wide range of research in social and life sciences.
  It generates artificial spatio-temporal (ST) point patterns 
  through the integration of microsimulation and agent-based models. 
  Allows a user to define the behaviours of a set of 'walkers' (agents, 
  objects, persons, etc,) whose interactions with the spatial (landscape) 
  and the temporal domains produce new point events. Based on the resulting
  point cloud, the ST patterns can be measured and utilized for spatial 
  and/or temporal model testings and evaluations. 

vignette: >
  %\VignetteIndexEntry{An R package for synthesizing spatiotemporal point patterns for social and life science research - A user guide}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---





## Introduction

In many research context, access to fine-grained spatiotemporal (ST) point data have been severely restricted due to privacy concerns. The `R-stppSim` package has been designed to address this challenge by presenting a framework that is capable of mimicking a real-life data through the integration of microsimulation and agent-based techniques. The framework comprises a set of 'walkers' (agents, objects, persons, etc.) with modifiable movement and interaction properties, constructed within specified spatial and temporal domains. As the framework produces new point events, in accordance with a user setting, a defined ST pattern emerges at both the local and global levels.

The package contains two main functions for synthesizing new datasets; (i) `psim_artif` and (ii) `psim_real`. The `psim_artif` synthesizes ST point patterns from scratch. That is, the simulation is entirely based on a user's setting. On the other hand, `psim_real` synthesizes ST point patterns based on a sample (or a sparse version) of a real data. The function first learns (or extracts) the certain ST characteristics of the sample data, and extrapolates to produce full dataset. The potential application fields for `stppSim` include criminology, epidemiology, and wildlife science.

## Simulation Parameters

The simulation parameters are in relation to three elements, namely; the '`walkers (agents)`', the `landscape` (spatial), and the `temporal dimension`. The parameters are described as follow: 

### Walkers (agents).

The walkers are defined primarily by the following characteristics:

* ***Origins*** - The walkers emanate from a set of origins that are distributed randomly across the landscape or origins defined from a sample of real point data. Origins are defined in terms of `xy` coordinates. In criminological application, a human offender can be modelled as a walker originating from his residence (origin). The `origins` of walkers typically exhibit two types of concentration: `nucleated` and `dispersed` (`ref`). A `nucleated` concentration is the one in which all origins concentrate around one focal point, while a `dispersed` concentration has no one particular focus, but could have multiple mini focal points (see fig. 1). 

\begin{figure}

\hfill{}\includegraphics[width=0.9\linewidth,height=1\textheight]{C:/Users/monsu/Documents/GitHub/stppSim backup/figs/origins} 

\caption{Interactive predictive hotspot map}\label{fig:fig1}
\end{figure}

* ***Movement*** - Walkers are configured to move in any direction and to be aware of obstacles (restrictions) on their path. The movements are controlled primarily by an in-built transition matrix (TM) definig two transitional states, namely; an `exploratory` state (in which a walker is merely exploring the environment) and a `performative` state (in which a walker is performing an action). The stochastic properties of the TM ensure variations in the behavioral patterns amongst the walkers.
In order to switch from one state to the next, a categorical distribution is assigned to the latent state variable $z_{it}$. So, every time step may be assigned to a movement behaviour state, independent of the previous state: $$z_t \sim Categorical(\Psi{_{1t}}, \Psi{_{2t}})$$ Such that $\Psi{_{i}}$ = Pr$(z_t = i)$, where $\Psi{_{i}}$ is the fixed probability of being in state $i$ at time $t$, and $\sum_{i=1}^{z}\Psi{_{i}}=1$

* ***Spatial threshold, k*** - The perception range of a walker at a given location. The parameter `k` is generally updated as a walker moves to a new location. A natural method for choosing the smoothing parameter is to plot out the data choose the estimate that is most in accordance with one's prior ideas about the `k` value. For many applications this approach will be perfectly satisfactory. To simulate data from sample data sets, the optimal value of `k` can also be extracted using a [plug-in function](https://www.taylorfrancis.com/books/mono/10.1201/9781315140919/density-estimation-statistics-data-analysis-silverman). 

* ***Steps*** - The maximum step taken by a walker as he moves from one location to another. This defines the speed of a walker across the space. The `steps` should be carefully defined, especially, when movements are restricted along narrow paths, such as route network (a step argument must be smaller than the width of the paths. 
* ***Proportional ratios*** - Defines the percentage of total events emanating from a small number of origins (i.e., most active origins). A `20:80` proportional ratios implies that 20% of origins (walkers) generates 80% of the point events.

### Landscape (spatial)

The followings are the key properties of a landscape:

* ***Boundary*** - A landscape is bounded - defined by a polygon shapefile (`poly`) or by the spatial extent of the sample point data.
* ***Restrictions*** - Defined as a raster layer. Typically defines two features: (i) Areas outside the boundary with the maximum restriction level (i.e., `1` - implying no movement), and (ii) Features within the boundary serving as obstructions to movement, e.g., certain land use type or topography, such as a fenced place and hills.
* ***Focal points*** - Locations around which there are higher concentration of opportunities (to event occurrences). Relatively higher activities in/around these locations, such as the centres of any city.

### Temporal dimension

The following parameters define the temporal dimension:

* ***Long-term trend*** - Defines the long-term direction of the time series to be simulated. This can be `stable`, `rising` or `falling`. If either `rising` or `falling`, the slope can be `gentle` or `steep`. Only specified when simulating from scratch. 
* ***Short-term patterns*** - Defines the short-term variation in events total over time. This is controlled by specifying the first seasonal peak point of the time series. A  `90` day first peak implies a seasonal cycle of `180` days. Also, specified when simulating from scratch.
* ***time bin*** - Time to reset all walkers.


## Installation of `stppSim`

From an R console, type:


```r
install.packages("stppSim")
#To install the developmental version of the package, type:
remotes::install_github("MAnalytics/stppSim")
```

Note: `remotes` is an extra package that needed to be installed prior to the running of this code. 


```r
#Now, load the package,
library(stppSim)
```


## Synthesizing `stpp` from scratch

To simulate point patterns from scratch, the argument `n_events` specifies the number of points to simulate. It is recommended to input a vector of values, instead of a single value. For example, `n_events = c(200, 500, 1000, 2000)`. This saves the user the time from having to synthesize new data. ***Note***: the specification of `n_events` input has little or no impacts on the computational time (See the manual for further details). 

### Example using a restricted landscape

Given a boundary shapefile {e.g., data(`camden_boundary`} and land use features with restriction values {e.g., data(`landuse`) - `Leisure` (0.5); `Sports` (0.7); and `Green` (0.9)}, the ST point pattern can be generated as follows: 


```r
#Using datasets that come with the package;
data(camden_boundary)
data(landuse)

#specifyings say 3 data sizes
pt_sizes = c(200, 1000, 2000)

artif_stpp <- psim_artif(n_events=pt_sizes, start_date = "2021-01-01",
  poly=camden_boundary, n_origin=50, resistance_feat = landuse,
  field = "rValue1",
  n_foci=5, foci_separation = 10, conc_type = "dispersed",
  p_ratio = 20, s_threshold = 50, step_length = 20,
  trend = "stable", first_pDate=NULL,
  slope = NULL,show.plot=FALSE, show.data=FALSE)
```

To preview the output:


```r
head(artif_stpp)
```

Users should explore the impacts of different arguments, including the `n_origin`, `foci_separation`, `trend` {`stable`,`increasing` or `decreasing`},  and `conc_type` {`nucleated` or `dispersed`}.  

Exploring the results using visualization:

* **Figure 1abc - spatial patterns of 200, 1000. 2000 data points**

The corresponding temporal pattern of the data can be visualized as follows:

* **Figure 2 - temporal patterns of 200, 1000. 2000 data points**


## Synthesizing `stpp` from a sample real dataset

Here, we are going to extract 30% random sample the `Theft` crime of Camden, then utilize the sample to synthesize a `full` data size.


```r
data(camden_crimes)

#get the 'theft' crime
theft <- camden_crimes %>%
  filter(type == "Theft")

#specify the proportion to extract
sample_size <- 0.3 #i.e. 30% of real data

set.seed(1000)
dat_sample <- theft[sample(1:nrow(theft),
  round((sample_size * nrow(theft)), digits=0),
  replace=FALSE),1:3]

#As a user would not normally know the actual size of 
#the real full data, therefore, we would assume `n_events`
#to be `2000`. In practice, a user should infer 
#the input size from any available (similar) data 
#from the study area.

#Now, simulate
sim_fullData <- psim_real(n_events=2000, ppt=dat_sample,
  start_date = NULL, poly = NULL, s_threshold = NULL,
  step_length = 20, n_origin=50, resistance_feat, field=NA,
  p_ratio=20, crsys = "EPSG:27700")

#plot(dat_sample$x, dat_sample$y) #preview

```

Preview the resulting data using visualization approach (above).

### Comparing simulated data and full real data

We will utilize a `visual approach` and a `statistical approach` to compare the spatial and temporal patterns of the simulated data and the `100%` real data size. The visual approach involves the comparison of `heat maps` derived from the aggregation of the datasets to a square grid system (250m x 250m). The statistical approach involves computing the `pearson coefficient` to compares the two datasets.

* Visual approach

** Spatial
Figure 1a (heat map of simulated data) 
Figure 1b (heat map of full real data)

** Temporal
Figure 2a (Time series plot of simulated data) 
Figure 2b (Time series plot of full real data)

* Statistical approach

Flattening the 2D square grids (above) and then apply Pearson correlation:


```r
#corr

cor(sim, real)

```


## Discussion

This vignette has demonstrated the basic utility of `stppSim` package for generating fine-grained spatiotemporal point patterns, (1) from scratch and (2) from a sample of real data sets. The parameters of the three simulation elements: the 'Walkers (agents)', the landscape (spatial), and the time dimension should be modified in accordance with the research questions at hand. The package has a wide potential areas of applications. Examples of objects with similar objectives as walkers (as defined in this package) include, human offenders, foraging animals and disease carriers, in victimization, Wildlife, and epidemiological studies, respectively. We will continue to update the package for wider application.   

We encourage users to report any bugs encountered while using the package so that they can be fixed immediately. Welcome contributions to this package which will be acknowledged accordingly. 