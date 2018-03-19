# TATI'S VERSION TO CROP TO BC
# data = CAN_adm1 (Whole map of Canada) or citiesCanada

stopifnot(packageVersion("SpaDES") >= "1.3.1.9010")

defineModule(sim, list(
  name = "cropReprojectLccAge",
  description = paste(
    "A translator module.",
    "Crops and reprojects the land cover classification from 2005 to a smaller,",
    "cropped RasterLayer, defined by ext, and with new projection defined by newCRS"
  ),
  keywords = c("translator", "lcc05", "Land Cover Classification", "vegetation"),
  childModules = character(),
  authors = c(person(c("Eliot", "J","B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))),
  version = numeric_version("1.1.3"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_,
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReprojectLccAge.Rmd"),
  reqdPkgs = list("archivist", "raster","rgeos", "parallel", "sp", "SpaDES"),
  parameters = rbind(
    defineParameter("useCache", "logical", FALSE, NA, NA,
                    "Should slow raster and sp functions use cached versions to speedup repeated calls"),
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA,
                    "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA,
                    "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA,
                    "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA,
                    "Interval between save events")),
  inputObjects = data.frame(
    objectName = c("lcc05", "age", "inputMapPolygon", "areaClass", "areaName","studyArea", "localGADMFilename", "polyMatrix", "areaSize"),
    objectClass = c("RasterLayer", "RasterLayer", "SpatialPolygons", "character", "character", "character", "character", "character", "character"),
    sourceURL = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    other = rep(NA_character_, 9L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("vegMapLcc", "ageMapInit"), #"cacheLoc", "mask", "crop", "projectRaster", "spTransform" as "functions"?
    objectClass = c("RasterLayer", "RasterLayer"),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE)
))

doEvent.cropReprojectLccAge <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      
      # do stuff for this event
      sim <- cropReprojectLccInit(sim)
      
      # schedule future event(s)
    },
    warning(paste(
      "Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
      "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""
    ))
  )
  return(invisible(sim))
}

### template initilization
cropReprojectLccInit <- function(sim) {
  if (proj4string(inputMapPolygon) != proj4string(lcc05)) {
    inputMapPolygon <- sp::spTransform(inputMapPolygon, CRS(proj4string(lcc05)))
  }
  
  totalArea <- rgeos::gArea(inputMapPolygon) / 1e4
  if (totalArea > 100e6) {
    stop("In the current implementation, please select another, smaller polygon",
         " (less than 100 million hectares).")
  }
 
  vegMapLcc2 <- crop(lcc05, inputMapPolygon)
  crs(vegMapLcc2) <- crs(sim$lcc05)
  
  sim$vegMapLcc <- mask(x = vegMapLcc2, mask = inputMapPolygon)
  setColors(sim$vegMapLcc, n = 256) <- getColors(sim$lcc05)[[1]] # mask removes colors!
  
  #if(ncell(sim$vegMapLcc)>5e5) beginCluster(min(parallel::detectCores(),6))
  
  # age will not run with projectRaster directly.
  # Instead, project the vegMap to age, then crop, then project back to vegMap.
  # vegMapLcc.crsAge <- cropReprojectLccAge$projectRaster(sim$vegMapLcc, crs = crs(sim$age))
  maskLayer <- Cache(sp::spTransform, x = sim$inputMapPolygon, CRSobj = crs(sim$age))
  age.crsAge2 <- Cache(raster::crop, sim$age, maskLayer)
  age.crsAge <- Cache(raster::mask, x = age.crsAge2, mask = maskLayer)
  sim$ageMapInit <- Cache(raster::projectRaster, from = age.crsAge,
                                                      to = sim$vegMapLcc,
                                                      method = "ngb")
  
  if (sum(!is.na(getValues(sim$ageMapInit))) == 0) {
    stop("There are no age data provided with input age map")
  }
  if (sum(!is.na(getValues(sim$vegMapLcc))) == 0) {
    stop("There are no vegatation data provided with input vegatation map")
  }
  setColors(sim$ageMapInit) <- colorRampPalette(c("light green", "dark green"))(50)
  
  #endCluster()
  
  return(invisible(sim))
}

# cropReprojectLccCacheFunctions <- function(sim) {
#   # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()
#   
#   if (P(sim)$useCache==TRUE) {
#     # Step 1 - create a location for the cached data if it doesn't already exist
#     sim$cacheLoc <- file.path(cachePath(sim), "cropReprojectLccAge") %>%
#       checkPath(create = TRUE)
#     if (!file.exists(file.path(sim$cacheLoc, "backpack.db"))) {
#       createEmptyRepo(sim$cacheLoc)
#     }
#     
#     # Step 2 - create a version of every function that is slow that includes the caching implicitly
#     sim$mask <- function(...) {
#       reproducible::Cache(cacheRepo = sim$cacheLoc, FUN = raster::mask, ...)
#     }
#     sim$crop <- function(...) {
#       reproducible::Cache(cacheRepo = sim$cacheLoc, FUN = raster::crop, ...)
#     }
#     sim$projectRaster <- function(...) {
#       reproducible::Cache(cacheRepo = sim$cacheLoc, FUN = raster::projectRaster, ...)
#     }
#     sim$spTransform <- function(...) {
#       reproducible::Cache(cacheRepo = sim$cacheLoc, FUN = sp::spTransform,  ...)
#     }
#   } else {
#     # Step 3 - create a non-caching version in case caching is not desired
#     sim$mask <- raster::mask
#     sim$crop <- raster::crop
#     sim$projectRaster <- raster::projectRaster
#     sim$spTransform <- sp::spTransform
#   }
#   
#   return(invisible(sim))
# }

### Inputs
.inputObjects <- function(sim) {
  if (is.null(sim$age) | is.null(sim$lcc05)) {
    if (!file.exists(file.path(modulePath(sim),"LccToBeaconsReclassify","data","LCC2005_V1_4a.tif"))){
      checksums1 <- downloadData("LccToBeaconsReclassify", file.path(modulePath(sim)))
      result1 <- checksums1[checksums1$expectedFile == "LCC2005_V1_4a.tif",]$result} else
        result1 <- "OK" # Not sure this is the right way of doing it. It is working for me, but I'm not sure check
      if (result1 != "OK" | is.na(result1)) {
        unzip(zipfile = file.path(modulePath(sim), "LccToBeaconsReclassify", "data", "LandCoverOfCanada2005_V1_4.zip"),
              files = "LCC2005_V1_4a.tif",
              exdir = file.path(modulePath(sim), "LccToBeaconsReclassify", "data"))
      }
      
      sim$age <- raster::raster(file.path(modulePath(sim), "forestAge", "data", "can_age04_1km.tif"))
      sim$lcc05 <- raster::raster(file.path(modulePath(sim), "LccToBeaconsReclassify", "data", "LCC2005_V1_4a.tif"))
      
      }

  #Territory, city or Polygon

  if (sim$studyArea == "defined"){
    if (sim$areaClass=="territory") {
      sim$localGADMFilename <- file.path(modulePath(sim), "warblersPointCountBC","data","CAN_adm1.RData")    
    }
    if (sim$areaClass=="city") {
      sim$localGADMFilename <- file.path(modulePath(sim), "warblersPointCountBC","data","citiesCanada.RData")
    }
    
    if (is.null(sim$areaClass)) {
      sim$areaClass <- "territory"
    }
    
    if (is.null(sim$areaName)) {
      sim$areaName <- "British Columbia"
    }
    
    if (is.null(sim$inputMapPolygon)) {
      sim$inputMapPolygon <- cropArea(areaClass = sim$areaClass, areaName = sim$areaName, localGADMFilename = sim$localGADMFilename)
    }

  }
  
  if (sim$studyArea == "random"){
    sim$inputMapPolygon <- randomPolygon(x = sim$polyMatrix, hectares = sim$areaSize)
  }
  
  return(sim)
}
