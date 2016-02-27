#' Function to colocate gauges with corresponding GPM pixel
#' 
#' @description xxx
#' 
#' @param gauges A STFDF object.
#' @param gpm a STFDF object.
#' @param resolution An integer describing the satellite spatial resolution.
#' @param longlat A logical object indicating if object is in long/lat or not. TRUE by default.
#' 
#' @details specifics of calculation (uses spdistsn1)
#' 
#' @return Object \code{gauges} with additional spatial column indicating GPM pixel ID.
#' 
#' @examples
#' #add(1, 1)
#' #add(10, 1)

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