shapefileitory <- function(areaName = sim$areaName, sim = sim, filePathTemplate = sim$filePathTemplate, 
                          rasterMap = sim$rasterMap, useSf = sim$useSf, 
                          croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat,
                          funcRast = sim$funcRast){

  require(reproducible)
  library(raster)
  library(gdalUtils)
  require(tools)
  require(rgdal)
  
  rasterMap <- raster(rasterMap)
  CAN_adm1 <- raster::getData("GADM", country="CAN", level=1, 
                                  path = dirname(filePathTemplate))
    shapefile <- load(filePathTemplate) %>%
     CAN_adm1[CAN_adm1$NAME_1 == areaName,]
    
if (!(sp::CRS(shapefile)==raster::crs(rasterMap))){
  shapefile <- sp::spTransform(x = shapefile, CRS = raster::crs(rasterMap))}
  
  ifelse(useSf==TRUE,{
    
    tryCatch(rasterMap[] <- rasterMap[])
    rgdal::writeOGR(shapefile, file.path(outputPath(sim), "reprojectedShapefile"), driver="ESRI Shapefile")
    cutlinePath <- file.path(outputPath(sim), "reprojectedShapefile.shp")
    
    gdalwarp(srcfile = rasterMap, # Raster file path
             dstfile = croppedRasterName, # Cropped raster file name
             overwrite=TRUE, # If you alreday have a raster with the same name and want to overwrite it
             cutline = cutlinePath, # Shapefile path to use for masking
             dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
             s_srs= as.character(crs(rasterMap)), #Projection from the source raster file
             t_srs= as.character(crs(rasterMap)), # Projection for the cropped file, it is possible to change projection here
             multi=TRUE, # Use multithreaded warping implementation.
             of=cropFormat, # Select the output format
             crop_to_cutline = TRUE, # Crop the raster to the shapefile
             tr=res(rasterMap)) # Raster resolution, not sure it needs to be the same from original raster

  },{
    shapefile <- sp::spTransform(x = shapefile, CRS = sp::CRS(rasterMap))
    eval(parse(text = paste0("raster::",funcRast,"(rasterMap, shapefile, filename=croppedRasterName)")))
  })
  
  newRasterMap <- raster(croppedRasterName)
  tryCatch(newRasterMap[] <- newRasterMap[]) # Bring the raster to memory (if possible)
  
  return(invisible(newRasterMap))
}