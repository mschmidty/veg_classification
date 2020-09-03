##library(lidR)
library(raster)
library(tidyverse)

#' This Script is used to calculate heights from a point cloud. 
#' 
#' @param las_path A character path to to the .las file or an object of type LAS loaded with the lidR package.
#' @return a raster with a

make_heights<-function(las_path){
  
  if(is.character(las_path)){
    ## Read Las files
    las<-readLAS(las_path)
  }else{
    las<-las_path
  }
  
  
  ## Calculate ground points form las
  ground<-lasground(las, csf(sloop_smooth = TRUE, 
                             rigidness = 2))
  ##calculate
  dtm <- grid_terrain(ground, 0.5, tin())
  dsm<-grid_canopy(ground, 0.5, p2r())
  
  rm(ground, las)
  
  ## combine the dtm and dsm
  chm<-brick(dtm, dsm)
  names(chm)<-c("dtm", "dsm")
  
  ##calculate the height by subtracting the digital terrain model or the canopy heights, by the digital surface model or the surfact values. 
  chm$height<-chm$dsm@data@values-chm$dtm@data@values
  values(chm$height)[values(chm$height) < (-5)] <- 0
  
  ##disaggregate the hights from 0.5m to 0.1m
  dis<-disaggregate(chm$height, fact=5)
  rm(dtm, dsm)
  return(dis)
}

#' This function takes merges the height raster with the tile. 
#' 
#' @param imagery_path file path, including extension, to the raster. 
#' @param height_raster_object raster of heights with the same resolution as the `imagery_path` file. They do not have to have the same extent, but they do need to overlap. 
#' @return a `raster::stacked` object with 5 layers `c('band1', 'band2', 'band3', 'band4')`.

### I altered this function so it might break some things!!!!
height_merge<-function(imagery_path, height_raster_object){
  raster<-brick(imagery_path)
  names(raster)<-c('band1', 'band2', 'band3', 'band4')
  
  if(crs(tile, asText = TRUE) != ifelse(
    is.na(crs(height, asText = TRUE)), 
    "No CRS", 
    crs(height, asText = TRUE)
  )){
    crs(height_raster_object)<-crs(raster)
  }
  
  height_raster_object<-height_raster_object%>%
    crop(raster[[1]])
  raster%>%
    crop(height_raster_object)%>%
    stack(height_raster_object)
}

height_merge2<-function(imagery_path, height_raster_object, norm=FALSE){
  raster<-brick(imagery_path)
  if(norm==TRUE){
    raster<-scale(raster, center = FALSE)
  }
  
  
  
  if(crs(raster, asText = TRUE) != ifelse(
    is.na(crs(height_raster_object, asText = TRUE)), 
    "No CRS", 
    crs(height_raster_object, asText = TRUE)
  )){
    crs(height_raster_object)<-crs(raster)
  }
  
  if(is.character(height_raster_object)){
    height_raster_object<-raster(height_raster_object)
  }
  
  height_raster_object<-height_raster_object%>%
    disaggregate(fact=5)%>%
    crop(raster[[1]], datatype = dataType(.))
  
  final_raster<-raster%>%
    crop(height_raster_object, datatype = dataType(.))%>%
    stack(height_raster_object)
}
# 
# imagery_path<-"DATA/Tiffs_W_SM/SM_GypsumGapNW_0103_180611.tif"
# r<-brick(imagery_path)
# 
# height_path<-"DATA/NOC_heights/WestSM_dZgridded/SM_GYPSUMGAPNW_0103_180611_dz.tif"
# h<-raster(height_path)
# 
# height_merge2(imagery_path, h)





#' This function combines the the `make_heights()` and `height_merge()` functions.
#' 
#' @param las_folder A character path to a folder full of .las files.
#' @param imagery_path raster of heights with the same resolution as the `r_path` file. They do not have to have the same extent, but they do need to overlap. 
#' @return a `raster::stacked` object with 5 layers `c('band1', 'band2', 'band3', 'band4')`.

calcadd_heights<-function(imagery_path, las_folder, output_folder){
  tryCatch({
    las_path<-lfn(imagery_path, las_folder)
    print(paste0("Finished lfn func: ", imagery_path))
    
    heights<-make_heights(las_path)
    print(paste0("Finished make_heights func: ", imagery_path))
    
    final_raster<-height_merge(imagery_path, heights)
    print(paste0("Finished height_merge func: ", imagery_path))
    
    writeRaster(final_raster, paste0(output_folder, "/height_add_", basename(imagery_path)))
    
  },error = function(error_message){
    t<-tibble(
      tile = imagery_path,
      status = "failed",
      error = error_message
    )
    write_csv(t, paste0(output_folder, "/failed_list.csv"), append = T)
    
  })
}

## Function to use for combining heights and 
combine_height_and_tile<-function(imagery_path, height_raster_folder, ouput_folder){
  
    las_path<-lfn_2(imagery_path, height_raster_folder)
    print(paste0("Finished lfn_2 func: ", imagery_path))
    
    heights<-st_read(las_path)
    print(paste0("Finished read heights", imagery_path))
    
    final_raster<-height_merge(imagery_path, heights)
    print(paste0("Finished height_merge func: ", imagery_path))
    
    writeRaster(final_raster, paste0(output_folder, "/height_add_", basename(imagery_path)))
    
}
