
# test to see if I can change the output resolution or if it needs to be exactly the same as the original raster in gdalwrap func.

library(SpaDES)

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
modules <- list("cropReproject")

## Set simulation and module parameters

times <- list(start = 1, end = 1, timeunit = "year")
parameters <- list(useSf = TRUE)
objects = list(rasterMap = NULL, 
               areaLimits = NULL, 
               areaName = NULL, 
               filePathTemplate = NULL, 
               polyMatrix = NULL, 
               areaSize = NULL, 
               croppedRasterName = NULL,
               funcRast = NULL) #"mask" or "crop"

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

