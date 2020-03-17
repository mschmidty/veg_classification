library(raster)
library(caret)
source("R/height_calc_funcs.R")

predict_raster<-function(vector_of_file_paths, model_path, height_raster_folder, output_folder = NULL){
  model <- readRDS(model_path)
  
  
  for(i in seq(1:length(vector_of_file_paths))){
    print(paste0("Starting ", basename(vector_of_file_paths[i]), " which is ", i, " out of ", length(vector_of_file_paths), ".", "TIME: ", Sys.time()))
    beginCluster(10)
    
    
    
    height_raster<-lfn_2(vector_of_file_paths[i], height_raster_folder)%>%
      raster()
    
    raster_to_predict<-height_merge2(vector_of_file_paths[i], height_raster)
    names(raster_to_predict)<-c("band1", "band2", "band3", "band4", "height")
    
    prediction<-clusterR(raster_to_predict, predict, args = list(model))
    
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
    endCluster()
    print(paste0("Tile ", basename(vector_of_file_paths[i]), " finished @: ", Sys.time()))
    
  }
}


##testing

tiles<-list.files("DATA/Tiffs_W_SM", pattern = ".tif", full.names = T)%>%
  sample(3)

start_time<-Sys.time()
predict_raster(tiles, 
               "model_runs/ID_CV_model_BS_UPDATE_normalized/ID_CV_model_BS_UPDATE3_normalized.rds", 
               "DATA/NOC_heights/WestSM_dZgridded")
Sys.time()-start_time



