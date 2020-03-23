library(raster)
rangify <- function(
  raster, 
  scale=TRUE){
  
  if(scale == TRUE){
    print ("scaling tile")
    
    beginCluster(10)
    raster<-clusterR(raster, scale, args = list(center = FALSE))
    endCluster()

  }else{
    range_func <- function(x){
      (x-cellStats(x,"min"))/(cellStats(x, "max")-cellStats(x, "min"))
    }
    #Commented out because I am currently testin parallelization at a higher level. 
    
    # if(is.numeric(parallel)){
    #   cl <- makePSOCKcluster(parallel)
    #   registerDoParallel(cl)
    #   
    #   foreach(i = 1:nlayers(raster)) %do% {
    #     raster[[i]]<-range_func(raster[[i]])
    #   }
    #   
    #   stopCluster(cl)
    # }else{
      for(i in seq(1:nlayers(raster))){
        print(i)
        raster[[i]]<-range_func(raster[[i]])
      }
    #}
    
  }
  
  
  raster
}
# imagery_path<-"DATA/Tiffs_W_SM/SM_GypsumGapNW_0103_180611.tif"
# r<-brick(imagery_path)
# 
# test<-rangify(r)
