library(raster)
rangify <- function(raster){
  range_func <- function(x){
    (x-cellStats(x,"min"))/(cellStats(x, "max")-cellStats(x, "min"))
  }
  
  for(i in seq(1:nlayers(raster))){
    print(i)
    raster[[i]]<-range_func(raster[[i]])
  }
  raster
}
