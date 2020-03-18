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
writeRaster(r_norm, "output/normalize_test/NORMALIZED_SM_GypsumGapNW_0103_180611.tif")

