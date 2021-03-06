cropArea <- function(areaName = sim$areaName, sim = sim, filePathTemplate = sim$filePathTemplate, 
                     rasterMap = sim$rasterMap, useGdal = sim$useGdal, 
                     croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                     funcRast = sim$funcRast){
  
  require(reproducible)
  library(raster)
  library(gdalUtils)
  require(tools)
  require(rgdal)
  require(sf)
  
  nRasters <- length(rasterMap)
  
  for (i in 1:nRasters){
    rasterMapEach <- raster(rasterMap[i])
    croppedRasterNameEach <- croppedRasterName[i]
    
    if(file.exists(croppedRasterNameEach)){next}
    
    if (!file.exists(file.path(outputPath(sim), "reprojectedShapefile.shp"))){
      CAN_adm1 <- raster::getData("GADM", country="CAN", level=1, 
                                  path = dirname(filePathTemplate))
      
      lsfiles <- file.info(dir(dirname(filePathTemplate)))
      fileTemplateName <- rownames(lsfiles[order(lsfiles$mtime),][1])
      filePathTemplate <- file.path(inputPath(sim), fileTemplateName)
      
      shapefile <- readRDS(filePathTemplate)
      shapefile <-  shapefile[shapefile$NAME_1 == areaName]
      
      if (!(sp::proj4string(shapefile)==as.character(raster::crs(rasterMapEach)))){
        shapefile <- sp::spTransform(x = shapefile, CRS = raster::crs(rasterMapEach))}
      shapefile <- as(shapefile, "SpatialPolygonsDataFrame")
      rgdal::writeOGR(obj = shapefile, dsn = file.path(outputPath(sim), "reprojectedShapefile.shp"), 
                      layer = "reprojectedShapefile", driver="ESRI Shapefile")}
    
    tryCatch(rasterMapEach[] <- rasterMapEach[])
    cutlinePath <- file.path(outputPath(sim), "reprojectedShapefile.shp")
    
    ifelse(useGdal==TRUE,{
      gdalwarp(srcfile = rasterMap[i], # Raster file path
               dstfile = croppedRasterNameEach, # Cropped raster file name
               overwrite = TRUE, # If you alreday have a raster with the same name and want to overwrite it
               cutline = cutlinePath, # Shapefile path to use for masking
               dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
               s_srs= as.character(crs(rasterMapEach)), #Projection from the source raster file
               t_srs= as.character(crs(rasterMapEach)), # Projection for the cropped file, it is possible to change projection here
               multi = TRUE, # Use multithreaded warping implementation.
               of = cropFormat, # Select the output format
               crop_to_cutline = TRUE, # Crop the raster to the shapefile
               tr = res(rasterMapEach)) # Raster resolution, not sure it needs to be the same from original raster
      
    },{
      
      shapefile <- sp::spTransform(x = shapefile, CRS = sp::CRS(rasterMapEach))
      eval(parse(text = paste0("raster::",funcRast,"(rasterMapEach, shapefile, filename=croppedRasterNameEach)")))
      
    })
    
  } # End for loop
  
  browser()
  
  newRasterMap <- lapply(X = croppedRasterName, FUN = function(dstFile){
    obj <- raster(dstFile)
    return(obj)
  })
  
  tryCatch(newRasterMap[] <- newRasterMap[]) # Bring the raster to memory (if possible)
  
  return(invisible(newRasterMap))
}