# cropReproject general module
# 
stopifnot(packageVersion("SpaDES") >= "1.3.1.9010")

defineModule(sim, list(
  name = "cropReproject",
  description = paste(
    "A translator module.",
    "It crops and/or reprojects a raster file to a smaller,",
    "cropped RasterLayer, defined by a shapefile that has iknformation on extent and projection.",
    "This was based on the cropReprojectLccAge SpaDES module"
  ),
  keywords = c("translator", "cropping", "raster crop", "shapefile", "crop to shapefile"),
  authors = c(person(c("Eliot", "J","B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
              person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9004", cropReproject = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReproject.Rmd"),
  reqdPkgs = list("archivist", "raster","rgeos", "parallel", "sp", "SpaDES"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("useSf", "logical", TRUE, NA, NA, "Should this module use the sf package for cropping?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = c("rasterMap", "cropPolygon", "areaLimits", "areaName", "filePathCropPolygon", "polyMatrix", "areaSize"), 
                 objectClass = c("rasterLayer", "SpatialPolygons", "character", "character", "character", "matrix", "numeric"), 
                 desc = c("Raster object/layer/map that you want to crop to your cropPolygon",
                          "Input polygon / shapefile for cropping the raster", paste("Choose between a 'defined' territory/shapefile or a 'random' polygon.", 
                                                                                     "If defined, chose between 'territory name' or 'polygon' to upload a shapefile."),
                          "Name of the territory to crop the raster to, or 'polygon' to provide a .shp file",
                          "File path to the shapefile data", "Random polygon matrix, if random polygon is to be used", "Area size if randomPolygon should be used"),
                 sourceURL = rep(NA, times = 7)
  ),
  outputObjects = bind_rows(
     createsOutput(objectName = "croppedRaster",
                  objectClass = "rasterLayer",
                  desc = "Raster object/layer/map cropped to your cropPolygon")
  )
))

doEvent.testing = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "cropReproject", "crop")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "cropReproject", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "cropReproject", "save")
    },
    crop = {
      
      if (sim$areaLimits=="defined"){
        
        ifelse (areaName == "polygon",
          sim$croppedRaster <- cropRaster(), # Create function cropRaster
          sim$croppedRaster <- cropArea(areaName = areaName, filePathCropPolygon = filePathCropPolygon, rasterMap = rasterMap, useSf = useSf))
      }
      
      if (sim$areaLimits=="random"){
        
        sim$croppedRaster <- cropRandomPolygon()) # Create function that will use the function randomPolygon to crop
      }
              
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "testing", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "testing", "save")
      
      # ! ----- STOP EDITING ----- ! #
    },
 
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above

