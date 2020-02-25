library(raster)
library(tidyverse)
library(sf)
## 
## Build datasets for running random forests. 



# Get Training tiles. 

#' `model_tiles()` reads a polygon shapefile of all of the tiles extents then joins that data with all training polygons. All tiles with training polygons within their extent are output as a vector to be passed to another function.
#' @param tile_shape_path path to shapefile of tiles, ending in .shp. 
#' @param training_poly_path path to shapefile of all training polygons.  

model_tiles<-function(tile_shape_path, training_poly_path){
  tiles<-st_read(tile_shape_path)%>%
    dplyr::select(NAME, Acres)
  train_polys<-st_read(training_poly_path)
  
  st_join(train_polys, tiles)%>%
    as_tibble()%>%
    distinct(NAME)%>%
    pull(NAME)%>%
    as.character()
}

# Subsetting training data

train_data<-function(imagery_name, height_imagery_folder, training_poly_path){
  
  path_to_raster<-list.files(height_imagery_folder, pattern = imagery_name, full.names = T)
  height_raster<-brick(path_to_raster)
  
  train_polys<-st_read(training_poly_path)%>%
    mutate(Id = as.numeric(rownames(.)))%>%
    st_crop(extent(height_raster))%>%
    mutate(Class = case_when(
      Class == "BG_Rock" ~ 1,
      Class == "Black_Sage" ~ 2, 
      Class == "Grass" ~ 3, 
      Class == "Other_Shrub" ~ 4,
      Class == "Other_Veg" ~ 5, 
      Class == "PJ" ~ 6, 
      Class == "Sage" ~ 7
    ))
  
  train_polys_class_raster<-rasterize(train_polys, height_raster, field = "Class", silent = T) 
  train_polys_id_raster<-rasterize(train_polys, height_raster, field = "Id", silent = T)
  
  comb<-stack(height_raster, train_polys_class_raster)%>%
    stack(train_polys_id_raster)
  
  print(names(comb))
  
  names(comb)<-c("band1", "band2", "band3", "band4", "height", "class", "id")
  
  dissolve_shape<-st_union(train_polys)
  
  raster::extract(comb, as_Spatial(dissolve_shape))
  
}

train_data_all<-function(training_poly_path, tile_shape_path, height_imagery_folder){
  tile_list<-model_tiles(tile_shape_path, training_poly_path)
  
  final_data_list<-sapply(tile_list, train_data, height_imagery_folder, training_poly_path, simplify = T)
  
  do.call(rbind, final_data_list)%>%
    as_tibble()
}





