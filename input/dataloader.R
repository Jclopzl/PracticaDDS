library("tidyr")

# 
# Load software inventory from all computers files included in the input.dir path
# 
# Usage: loadComputerSotware <input.dir>
#
# @input.dir Path containing software computer input files
#
loadComputerSoftware <- function(input.dir) {
  
  
  
  for (file in list.files(input.dir)) {
    computer.entries <- read.csv(paste(input.dir,file,sep="/"),header=TRUE,sep=";",stringsAsFactors = FALSE)
    computer.entries$computer <- rep(file,nrow(computer.entries))
    
    if (exists('all.computers.entries')) {
      all.computers.entries <- dplyr::union(all.computers.entries,computer.entries)
    } else {
      all.computers.entries <- computer.entries
    }
  }
  
  names(all.computers.entries) <- c("name","version","vendor","computer") 
  
  return(all.computers.entries)

}

downloadSysdata <- function(input.dir) {
  
  url <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  path <- paste(input.dir,"sysdata.rda",sep = "/")
  download.file(url = url, path)
  
}

loadComputerCriticity <- function(input.file, computer.entries=NULL, random=FALSE) {
  
  computers.entries.criticity <- as.data.frame(x=c())
  
  if( random && !is.null(computers.entries)) {
    # Generación aleatoria de criticidad
    computers.entries.criticity <- dplyr::select(computers.entries,computer) %>% distinct() 
    computers.entries.criticity$criticidad <- as.data.frame(sample(x=rep(x=1:nrow(computers.entries.criticity)%%2)))
  } else {
    computers.entries.criticity <- read.csv(input.file,header=TRUE,sep=";",stringsAsFactors = FALSE)
  }
 
  names(computers.entries.criticity) <- c("computer","criticidad")
  
  return(computers.entries.criticity)
  
}

