# global.R
# 
# Global needs to have the following:
# 1. Allow for an example of the module: download data online (both raster and shapefile)
# 2. Need to allow for using both raster or gdal packages for cropping
# 3. Needs to be flexible (message) user on where to store the files if they have it or turn on the example files
# 4. Chamge crs from randomPolygon function

# BELOW TO MODIFY

library(SpaDES)

## decide where you're working
# set the directories
workDirectory <- getwd()

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
modules <- list("cropReproject") # 

## Set simulation and module parameters

times <- list(start = 2009, end = 2020, timeunit = "year")
parameters <- list(
  .globals = list(.plotInitialTime = 1),
  warblersPointCountBC = list(overrideModel = TRUE, start = 2009, end = 2011)
)
objects = list(areaLimits = "random", areaClass = "territory", areaName = "British Columbia", 
               polyMatrix = matrix(c(-122.85, 52.04), ncol = 2), areaSize = 500000,
               species = c("PISI","UEFL","YRWA","DEJU"))

useSf = TRUE # <-------------------------------

# objects = list(areaLimits = "random")

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE) #c("warblersPointCountBC","init")

