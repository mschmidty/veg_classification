library(raster)
library(caret)
source("R/height_calc_funcs.R")

predict_raster<-function(vector_of_file_paths, model_path, height_raster_folder, output_folder = NULL){
  model <- readRDS(model_path)
  
  
  for(i in seq(1:length(vector_of_file_paths))){
    print(paste0("Starting ", basename(vector_of_file_paths[i]), " which is ", i, " out of ", length(vector_of_file_paths), ".", "TIME: ", Sys.time()))
    
    
    
    
    height_raster<-lfn_2(vector_of_file_paths[i], height_raster_folder)%>%
      raster()
    
    raster_to_predict<-height_merge2(vector_of_file_paths[i], height_raster)
    if(nlayers(raster_to_predict)==5){
      names(raster_to_predict)<-c("band1", "band2", "band3", "band4", "height")
    }else{
      names(raster_to_predict)<-c("band1", "band2", "band3", "band4", "MSAVI2","height")
    }

    
    prediction<-predict(raster_to_predict, model)
    
    output_path<-dirname(model_path)
    
    if(!is.null(output_folder)){
      output_path = output_folder
    }
    
    output_path<-paste0(output_path,
                       "/MODEL_",
                       sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(model_path)), 
                       "_TILE_",
                       sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(vector_of_file_paths[i])),
                       ".tif"
                       )
    
    
    writeRaster(prediction, output_path)
    
    print(paste0("Tile ", basename(vector_of_file_paths[i]), " finished @: ", Sys.time()))
    
  }
}

<<<<<<< HEAD

predict_raster2<-function(tile_path, model, height_raster_folder, model_name, output_folder = NULL){
  print(paste0("Starting ", tile_path, ".", "TIME: ", Sys.time()))
  
  
  
  
  height_raster<-lfn_2(tile_path, height_raster_folder)%>%
    raster()
  
  raster_to_predict<-height_merge2(tile_path, height_raster)
  names(raster_to_predict)<-c("band1", "band2", "band3", "band4", "height", "MSAVI2")
  
  
  
  prediction<-predict(raster_to_predict, model)
  
  
  if(!is.null(output_folder)){
    output_path = output_folder
  }
  
  output_path<-paste0(output_path,
                      "/MODEL_",
                      model_name, 
                      "_TILE_",
                      sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tile_path)),
                      ".tif"
  )
  
  
  writeRaster(prediction, output_path)
  
  print(paste0("Tile ", tile_path, " finished @: ", Sys.time()))
}





=======
>>>>>>> bca64543fc5c9353469bdd2ee4ebec38d291cc02
