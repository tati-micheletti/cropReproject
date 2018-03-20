cropTerritory <- function(areaClass, areaName, filePathCropPolygon, rasterMap, useSf) {
  
  require(raster)
  

    try(if (!file.exists(filePathCropPolygon)) {
      CAN_adm1 <- raster::getData("GADM", country="CAN", level=1, 
                                  path = dirname(filePathCropPolygon))
    }
  )
    cropTerr <- load(filePathCropPolygon) %>%
     CAN_adm1[CAN_adm1$NAME_1 == areaName,]}
  
cropTerr <- sp::spTransform(x = cropTerr, CRS = sp::CRS(rasterMap))
  
  return(invisible(cropTerr))
}