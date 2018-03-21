---
title: "cropReproject"
author: "Tati Micheletti"
date: "March 20, 2018"
output: html_document
---

## cropReproject SpaDES module

This is a global script for the cropReproject SpaDES module. This module focused on cropping and reprojecting rasters to rasters or shapefiles.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Global script for the module
#### LOad SpaDES, set the directories and paths 

```{r dir}
library(SpaDES)

workDirectory <- getwd()

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)
```

#### List module name
```{r module}
modules <- list("cropReproject")
```

#### Set simulation and module parameters, where:

*rasterMap* = Raster object/layer/map file path that you want to crop (character string);  
*areaLimits* =  Choose between a 'defined' territory/shapefile/raster or a 'random' polygon (character string);  
*areaName* = If using canadian territories to crop, name the territory (character string);  
*filePathTemplate* = Full path of the template for cropping  (character string);  
*polyMatrix* = If using a random polygon for cropping, center point as latlong coordinates;  
*areaSize* = If using a random polygon for cropping, size of the area around the center point defined by polyMatrix;  
*croppedRasterName* = File path and name for the cropped raster  (character string);  
*funcRast* = function if using raster package. Can take "mask" or "crop" (character string).  

```{r params}
times <- list(start = 1, end = 1, timeunit = "year")
parameters <- list(useSf = FALSE)
objects = list(rasterMap = NULL,
               areaLimits = NULL, 
               areaName = NULL, 
               filePathTemplate = NULL, 
               polyMatrix = NULL, 
               areaSize = NULL, 
               croppedRasterName = NULL,
               funcRast = NULL)
```

#### For using external graph device
```{r graph}
dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()
```

#### Simulation setup and SpaDES call
```{r simulation}
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))
```
