#############################################################################################
# title         : Perform a Mann-Kendall trend test of satellite image time series
# purpose       : Mann-Kendall test of monotonic trend and associated statistical significance
# author        : Abdulhakim Abdi (@HakimAbdi)
# input         : Raster stack/brick comprising a time series of observations
# output        : Raster object of monotoic trend (Kendall's tau), significance or both (i.e. brick) 
# data          : Test data: https://1drv.ms/u/s!AsHsKb_qtbkwg4ti8pJCxvUer9rMqg?e=BGcH1K
# notes         : Depending on the time series, it is might be useful prewhiten the data to remove serial correlation 
#               : prior to extracting the trend. See blog post on www.hakimabdi.com for more information. 
#############################################################################################

MKraster <- function(rasterstack, type=c("trend","pval","both")){
  
  # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
  # e.g. nlayers(rasterstack), ncell(rasterstack) etc.
  print(paste("Start MKraster:",Sys.time()))
  print("Loading parameters")
  layers=nlayers(rasterstack);ncell=ncell(rasterstack);
  ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
  extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
  print("Done loading parameters")
  mtrx <- as.matrix(rasterstack,ncol=layers)
  empt <- matrix(nrow=ncell, ncol=2)
  
  print("Initiating loop operation")
  if (type == "trend"){
    
    for (i in 1:length(mtrx[,1])){
      if (all(is.na(mtrx[i,]))){ 
        empt[i,1] <- NA 
      } else 
        if (sum(!is.na(mtrx[i,])) < 4){
          empt[i,1] <- NA 
        } else 
          empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$tau)
    }
    
    print("Creating empty raster")
    trend <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(trend) <- extent
    print("Populating trend raster")
    values(trend) <- empt[,1]
    print(paste("Ending MKraster on",Sys.time()))
    trend
  } 
  else
    if (type == "pval"){
      
      for (i in 1:length(mtrx[,1])){
        if (all(is.na(mtrx[i,]))){ 
          empt[i,1] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,])) < 4){
            empt[i,1] <- NA 
          } else 
            empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$sl)
      }
      
      pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
      extent(pval) <- extent
      print("Populating significance raster")
      values(pval) <- empt[,1]
      print(paste("Ending MKraster on",Sys.time()))
      pval
    }
  else
    if (type == "both"){
      
      for (i in 1:length(mtrx[,1])){
        if (all(is.na(mtrx[i,]))){ 
          empt[i,1] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,])) < 4){
            empt[i,1] <- NA 
          } else 
            tryCatch({
              empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$tau)
              empt[i,2] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$sl)
            })
      }
      
      tr <- raster(nrows=nrow,ncols=ncol,crs=crs)
      pv <- raster(nrows=nrow,ncols=ncol,crs=crs)
      print("Populating raster brick")
      values(tr) <- empt[,1]
      values(pv) <- empt[,2]
      brk <- brick(tr,pv)
      extent(brk) <- extent
      names(brk) <- c("trend","p.value")
      print(paste("Ending MKraster on",Sys.time()))
      brk
    }
}