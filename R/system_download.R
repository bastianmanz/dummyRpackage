#' Function to download GPM-IMERG HDF5 files from NASA PPS
#' 
#' @description xxx
#' 
#' @param product A character string describing which GPM product is selected
#' @param nrt_product A character string describing whih NRT GPM product is used.
#' @param OS A character string describing the operating system.
#' 
#' @details requires file list of URLs to download HDF files using wget/ curl. 
#' 
#' @return Downloads and writes HDF files. No R object created.
#' 
#' @examples
#' #add(1, 1)
#' #add(10, 1)

system_download <- function(product,nrt_product,OS){
  
  # read file names
  if(product=="nrt"){
    filenames <- read.table(paste("file_list_",nrt_product,".txt",sep=""))
  } else{
    filenames <- read.table(paste("file_list_",product,".txt",sep=""))
  }
  filenames <- data.frame(filenames[1:(nrow(filenames)-1),])
  
  # system-based download
  
  for(i in 1:nrow(filenames)){
        
    fi <- filenames[i,1]
    
    fileConn<-file("fi.txt")
    writeLines(as.character(fi), fileConn)
    close(fileConn)  
    
    if(OS %in% c("linux", "mac")){
      system("xargs -n 1 curl -O < fi.txt") # xargs -n 1 curl -O < myfile.dat
      file.remove("fi.txt")
    } else if(OS=="windows"){
      system(" wget -i fi.txt") # wget -i myfile.dat
      file.remove("fi.txt")
    }
     
  }
}
  
  
