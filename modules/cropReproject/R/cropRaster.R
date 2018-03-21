# cropRaster function

cropRaster <- function(sim = sim, filePathTemplate = sim$filePathTemplate, 
                       rasterMap = sim$rasterMap, useSf = sim$useSf, 
                       croppedRasterName =sim$croppedRasterName, cropFormat = sim$cropFormat){
  
  library(raster)
  library(gdalUtils)
  require(tools)
  require(rgdal)
  
  rasterMap <- raster(rasterMap)
  
  shaperaster <- raster::raster(filePathTemplate)
  if (!(sp::proj4string(shapefile)==as.character(raster::crs(rasterMap)))){
    shaperaster <- sp::spTransform(x = shaperaster, CRS = raster::crs(rasterMap))}
    
  if(useSf==TRUE){ # sf package
    
    tryCatch(rasterMap[] <- rasterMap[])
    raster::writeRaster(shaperaster, file.path(outputPath(sim), "reprojectedRasterfile"), format = cropFormat)
    cutlinePath <- file.path(outputPath(sim), paste0("reprojectedRasterfile", cropFormat))
    
            gdalwarp(srcfile = sim$rasterMap, # Raster file path
                      dstfile = croppedRasterName, # Cropped raster file name
                      overwrite=TRUE, # If you alreday have a raster with the same name and want to overwrite it
                      cutline = cutlinePath, # shaperaster path to use for masking
                      dstalpha = TRUE, # Creates an output alpha band to identify nodata (unset/transparent) pixels
                      s_srs= as.character(crs(rasterMap)), #Projection from the source raster file
                      t_srs= as.character(crs(rasterMap)), # Projection for the cropped file, it is possible to change projection here
                      multi=TRUE, # Use multithreaded warping implementation.
                      of=cropFormat, # Select the output format
                      crop_to_cutline = TRUE, # Crop the raster to the shaperaster
                      tr=res(rasterMap)) # Raster resolution, not sure it needs to be the same from original raster

    } else { # raster package
      
      eval(parse(text = paste0("raster::",funcRast,"(rasterMap, shaperaster, filename=croppedRasterName)")))
    }
  
  newRasterMap <- raster(croppedRasterName)
  tryCatch(newRasterMap[] <- newRasterMap[]) # Bring the raster to memory (if possible)
  
  return(invisible(newRasterMap))
}