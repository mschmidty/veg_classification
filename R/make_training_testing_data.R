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

train_data<-function(imagery_name, imagery_folder, training_poly_path){
  
  path_to_raster<-list.files(imagery_folder, pattern = imagery_name, full.names = T)
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

train_data2<-function(imagery_name, imagery_folder, height_raster_folder, training_poly_path){
  
  
  tile_path<-paste(imagery_folder, imagery_name, sep = "/")
  
  height_raster_path<-lfn_2(tile_path, height_raster_folder)
  height_raster<-raster::raster(height_raster_path)
  
  height_tile_merge<-height_merge2(tile_path, height_raster)
  
  print(paste0("Load raster and add height: ", tile_path))
  
  train_polys<-sf::st_read(training_poly_path)%>%
    mutate(Id = as.numeric(rownames(.)))%>%
    sf::st_crop(extent(height_tile_merge[[1]]))%>%
    dplyr::mutate(Class = case_when(
      Class == "BG_Rock" ~ 1,
      Class == "Black_Sage" ~ 2, 
      Class %in% c("Grass", "Blue_Grama") ~ 3, 
      Class %in% c("Other_Shrub", "Rock_Goldenrod", "Winterfat", "Greasewood", "Fourwing Saltbush") ~ 4,
      Class == "Other_Veg" ~ 5, 
      Class == "PJ" ~ 6, 
      Class == "Sage" ~ 7,
      Class == "Shadow" ~ 8
    ))
  
  train_polys_class_raster<-raster::rasterize(train_polys, height_tile_merge[[1]], field = "Class", silent = T) 
  train_polys_id_raster<-raster::rasterize(train_polys, height_tile_merge[[1]], field = "Id", silent = T)
  
  print("rasterized complete")
  
  comb<-stack(height_tile_merge, train_polys_class_raster)%>%
    stack(train_polys_id_raster)
  
  if(nlayers(comb)==7){
    names(comb)<-c("band1", "band2", "band3", "band4", "height", "Class", "Id")
  }else{
    names(comb)<-c("band1", "band2", "band3", "band4", "MSAVI2","height", "Class", "Id")
  }
  
  print("rename complete")
  
  
  dissolve_shape<-sf::st_union(train_polys)
  
  print("dissolve complete")
  
  raster::extract(comb, as_Spatial(dissolve_shape))[[1]]
}

train_data3<-function(imagery_name, imagery_folder, height_raster_folder, training_poly_path){
  
  tile_path<-paste(imagery_folder, imagery_name, sep = "/")
  
  lfn_2<-function(imagery_path, height_raster_folder){
    
    name_parts<-imagery_path%>%
      basename()%>%
      sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(.))%>%
      stringr::str_split(pattern="_")
    
    list.files(height_raster_folder,
               pattern = paste(name_parts[[1]][1], name_parts[[1]][2], name_parts[[1]][3], sep = "_"),
               ignore.case = T,
               full.names = T)%>%
      .[matches(".tif$", vars=.)]
  }
  
  height_raster_path<-lfn_2(tile_path, height_raster_folder)
  height_raster<-raster::raster(height_raster_path)
  
  height_merge2<-function(imagery_path, height_raster_object){
    raster<-raster::scale(raster::brick(imagery_path), center = FALSE)
    
    
    if(raster::crs(raster, asText = TRUE) != ifelse(
      is.na(raster::crs(height_raster_object, asText = TRUE)), 
      "No CRS", 
      raster::crs(height_raster_object, asText = TRUE)
    )){
      raster::crs(height_raster_object)<-raster::crs(raster)
    }
    
    if(is.character(height_raster_object)){
      height_raster_object<-raster::raster(height_raster_object)
    }
    
    height_raster_object<-height_raster_object%>%
      raster::disaggregate(fact=5)%>%
      raster::crop(raster[[1]], datatype = raster::dataType(.))
    
    final_raster<-raster%>%
      raster::crop(height_raster_object, datatype = raster::dataType(.))%>%
      raster::stack(height_raster_object)
  }
  
  height_tile_merge<-height_merge2(tile_path, height_raster)
  
  train_polys<-sf::st_read(training_poly_path)%>%
    dplyr::mutate(Id = as.numeric(rownames(.)))%>%
    sf::st_crop(raster::extent(height_tile_merge[[1]]))%>%
    dplyr::mutate(Class = dplyr::case_when(
      Class == "BG_Rock" ~ 1,
      Class == "Black_Sage" ~ 2, 
      Class %in% c("Grass", "Blue_Grama") ~ 3, 
      Class %in% c("Other_Shrub", "Rock_Goldenrod", "Winterfat", "Greasewood", "Fourwing Saltbush") ~ 4,
      Class == "Other_Veg" ~ 5, 
      Class == "PJ" ~ 6, 
      Class == "Sage" ~ 7
    ))
  
  train_polys_class_raster<-raster::rasterize(train_polys, height_tile_merge[[1]], field = "Class", silent = T) 
  train_polys_id_raster<-raster::rasterize(train_polys, height_tile_merge[[1]], field = "Id", silent = T)
  
  comb<-raster::stack(height_tile_merge, train_polys_class_raster)%>%
    raster::stack(train_polys_id_raster)
  
  names(comb)<-c("band1", "band2", "band3", "band4", "height", "Class", "Id")
  
  dissolve_shape<-sf::st_union(train_polys)
  
  raster::extract(comb, sf::as_Spatial(dissolve_shape))[[1]]%>%
    tibble::as_tibble()
}


train_data_all<-function(training_poly_path, tile_shape_path, height_imagery_folder){
  tile_list<-model_tiles(tile_shape_path, training_poly_path)
  
  final_data_list<-sapply(tile_list, train_data2, height_imagery_folder, training_poly_path, simplify = T)
  
  do.call(rbind, final_data_list)%>%
    as_tibble()
}

train_data_all_add_heights<-function(
  training_poly_path, 
  tile_shape_path, 
  imagery_folder, 
  height_raster_folder, 
  parallel=FALSE
  ){
  tile_list<-model_tiles(tile_shape_path, training_poly_path)
  
  list_length<-length(tile_list)
  
  lapply(tile_list, train_data2, imagery_folder, height_raster_folder, training_poly_path)
  
  
  ##final_data_list<-sapply(tile_list, train_data2, imagery_folder, height_raster_folder, training_poly_path, simplify = T)
  
  # do.call(rbind, final_data_list)%>%
  #   as_tibble()
}
