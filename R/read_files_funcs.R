lfn<-function(imagery_path, las_folder){
  
  name_parts<-imagery_path%>%
    basename()%>%
    str_split(pattern="_")
  name_parts
  
  list.files(las_folder,
             pattern = paste(name_parts[[1]][1], name_parts[[1]][2], name_parts[[1]][3], sep = "_"),
             ignore.case = T,
             full.names = T)
}

l_train_f<-function(training_poly_path, tile_poly_path){
  
  train_polys<-st_read(training_poly_path)
  
  tiles<-st_read(tile_poly_path)%>%
    select(NAME, Acres)
  
  st_join(train_polys, tiles)%>%
    as_tibble()%>%
    distinct(NAME)%>%
    pull(NAME)
}

lfn_2<-function(imagery_path, height_raster_folder){
  
  name_parts<-imagery_path%>%
    basename()%>%
    sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(.))%>%
    str_split(pattern="_")
  
  list.files(height_raster_folder,
             pattern = paste(name_parts[[1]][1], name_parts[[1]][2], name_parts[[1]][3], sep = "_"),
             ignore.case = T,
             full.names = T)%>%
             .[matches(".tif$", vars=.)]
}

