orthomap <img src="img/orthomap-sticker.png" height="120" align="right"/>
=========================================================

[![Build Status](https://travis-ci.org/ahasverus/orthomap.svg?branch=master)](https://travis-ci.org/ahasverus/orthomap) [![](https://img.shields.io/badge/licence-GPLv3-8f10cb.svg)](http://www.gnu.org/licenses/gpl.html)

Overview
--------

The package **orthomap** contains one single function: `orthomap()`. It was designed to solve a common issue encountered when using the `map()` function of the [**maps**](http://cran.r-project.org/web/packages/maps/index.html) package in the orthographic projection: some polygons are not correctly projected and some geographical aberrations appear in the final map (see figure below and pay attention to Russia and Antarctica). Moreover, the `orthomap()` function returns projected World country polygons into the `SpatialPolygons` format.

![figure-1](img/figure-1.png)

Installation
--------

To install the package **orthomap** from GitHub, first install the package [**devtools**](http://cran.r-project.org/web/packages/devtools/index.html) from the CRAN.

```r
### Install the < devtools > package
install.packages("devtools", dependencies = TRUE)

### Load the < devtools > package
library(devtools)
```

Then install the **orthomap** package:

```r
### Install the < orthomap > package from GitHub
devtools::install_github("ahasverus/orthomap")

### Load the < orthomap > package
library(orthomap)
```

Getting started
--------

Some useful command lines to get started:

```r
### List the content (objects and functions) of the < orthomap > package
ls("package:orthomap")

### Open the < orthomap > package home page
help(package = "orthomap")

### Open the help file of a specific function
help(orthomap)
```

Usage and workflow
--------

```r
### World orthographic map with default settings
world <- orthomap(
  query      = NULL,
  centre     = c(0, 0),
  border     = NA,
  fill       = "#909090",
  grid       = TRUE,
  grid.color = "#969696",
  grid.type  = 1,
  grid.size  = 0.25
)

### Short version
world <- orthomap()

### World orthographic map centered on Sao Tome and Principe
world <- orthomap(query = "Sao Tome and Principe")

### Class of the object returned
class(world)
# [1] "SpatialPolygons"
# attr(,"package")
# [1] "sp"

### Number of polygons
length(world)
# [1] 833

### World map centered on the North Pole
world <- orthomap(centre = c(90, 0))

### World map with customized graphical parameters
world <- orthomap(centre = c(90, 0), mar = rep(0, 4), bg = "black")

### To customize other graphical parameters, see:
?par
```

Acknowledgments
--------

The development of this function was inspired by the post of David on [Stack Overflow](https://stackoverflow.com/questions/35911452/clipping-polygons-when-drawing-world-map-in-orthographic-projection). Thank you David!
