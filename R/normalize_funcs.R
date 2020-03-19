library(raster)
library(doParallel)
rangify <- function(raster, parallel = FALSE){
  range_func <- function(x){
    (x-cellStats(x,"min"))/(cellStats(x, "max")-cellStats(x, "min"))
  }
  
  if(is.numeric(parallel)){
    cl <- makePSOCKcluster(parallel)
    registerDoParallel(cl)

    foreach(i = 1:nlayers(raster)) %do% {
      raster[[i]]<-range_func(raster[[i]])
    }
    
    stopCluster(cl)
  }else{
    for(i in seq(1:nlayers(raster))){
      print(i)
      raster[[i]]<-range_func(raster[[i]])
    }
  }
  
  raster
}

source("R/load_funcs.R")


imagery_path<-"DATA/SanMiguel_West/Tiffs/SM_GypsumGapNW_0103_180611.tif"
r<-brick(imagery_path)
r_norm<-rangify(r, 4)

imagery_path<-"DATA/SanMiguel_West/Tiffs/SM_GypsumGapNW_0104_180611.tif"
r<-brick(imagery_path)
r_norm2<-rangify(r, 4)

writeRaster(r_norm2, "output/normalize_test/NORMALIZED_SM_GypsumGapNW_0104_180611.tif")

writeRaster(r_norm, "output/normalize_test/NORMALIZED_SM_GypsumGapNW_0103_180611.tif", overwrite = T)

library(tidyverse)
r_norm_c<-r_norm%>%
  crop(extent(r_norm2))%>%
  as.data.frame()%>%
  as_tibble()

r_norm2_c<-r_norm2%>%
  crop(extent(r_norm))%>%
  as.data.frame()%>%
  as_tibble()

reshaped <- r_norm2_c%>%
  set_names(c("band1", "band2", "band3", "band4"))%>%
  mutate(tile = "GypsumGapNW_0104")%>%
  bind_rows(
    r_norm_c%>%
      set_names(c("band1", "band2", "band3", "band4"))%>%
      mutate(tile = "GypsumGapNW_0103")
  )%>%
  pivot_longer(-tile, names_to = "band", values_to = "values")

reshaped%>%
  group_by(tile)%>%
  sample_frac(0.1)%>%
  ungroup()%>%
  ggplot(aes(band, values, color = tile))+
  geom_boxplot()+
  theme_light()+
  labs(
    title = "Differences after normalization from two tiles that are overlapping",
    subtitle = "10% of the population.",
    x = "Band",
    y = "Normalized Values"
  )
  