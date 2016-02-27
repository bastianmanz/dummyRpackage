## function to colocate gauges with corresponding GPM pixel

## Author: Bastian Manz, Imperial College London
## Date: 01/02/2016
## Use: open-source and open-access (free)

colocate <- function(gauges, gpm, resolution,longlat=TRUE){
  
  pts <- gauges@sp
  coords <- gpm@sp
  
  # get location of gauges in gpm pixels
  loc1 <- numeric()
  loc_dists <- numeric()
  
  # set progress bar
  pb <- txtProgressBar(style=3)
  print("Colocate gauges and sat pixels")
  
  for (i in 1:length(pts)) {
    setTxtProgressBar(pb, i/length(pts))
    loc1[i] <- which.min(spDistsN1(coords,pts[i,],longlat))
    loc_dists[i] <- min(spDistsN1(coords,pts[i,] ,longlat))
  }
  
  # remove gauges which have no pixel within length of pixel radius
  if(longlat==TRUE){
    x=sqrt(2*((resolution*100)^2)) # pythagoras, convert from deg latlong to km
  } else{
    x=sqrt(2*(resolution^2)) # pythagoras
  }
  loc <- loc1[c(which(loc_dists < x))]
  
  gauges@sp@data <- cbind(gauges@sp@data,loc)
  colnames(gauges@sp@data)[ncol(gauges@sp)] <- "GPM_PIXEL"
  
  return(gauges) 
}