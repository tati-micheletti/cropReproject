
cropArea <- function(areaClass, areaName, localGADMFilename) {
  
  require(raster)
  
  if (areaClass == "territory") {
    try(if (!file.exists(localGADMFilename)) {
      CAN_adm1 <- raster::getData("GADM", country="CAN", level=1, 
                          path = dirname(localGADMFilename))
    }
    )
    load(localGADMFilename)
    inputMapPolygon <- CAN_adm1[CAN_adm1$NAME_1 == areaName,]}
  
  if (areaClass == "city") {
    
    try(if (!file.exists(localGADMFilename)){
      temp <- tempfile()
      dataDir <- dirname(localGADMFilename)
      download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/lcsd000a17a_e.zip",temp)
      unzip(temp, exdir = dataDir)
      localGADMFilename <- shapefile(file.path(dataDir,"lcsd000a17a_e.shp"))
    }
    )

    load(file.path(getPaths()$modulePath,"cropReprojectLccAge/data","citiesCanada.RData"))
    inputMapPolygon <- cities[cities$CSDNAME == areaName,]
    }
  inputMapPolygon <- sp::spTransform(x = inputMapPolygon, CRS = sp::CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")) # CHANGE IF NOT WORKING WITH LCC05 PROJECTION!
  
  return(invisible(inputMapPolygon))
}
