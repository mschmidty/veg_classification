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
