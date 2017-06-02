library("tidyr")

# 
# Load software inventory from all computers files included in the inputDir path
# 
# Usage: loadComputerSotware <inputDir>
#
# @inputDir Path containing software computer input files
#
loadComputerSoftware <- function(inputDir) {
  
  
  
  for (file in list.files(inputDir)) {
    computer.entries <- read.csv(paste(inputDir,file,sep="/"),header=TRUE,sep=";",stringsAsFactors = FALSE)
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

downloadSysdata <- function(inputDir) {
  
  url <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  path <- paste(inputDir,"sysdata.rda",sep = "/")
  download.file(url = url, path)
  
}

loadComputerCriticity <- function(computers.entries) {
  
  # Generación aleatoria de criticidad
  computers.entries.criticity.rand <- dplyr::select(computers.entries,computer) %>% distinct() 
  computers.entries.criticity.rand$criticidad <- as.data.frame(sample(x=rep(x=1:nrow(computers.entries.criticity.rand)%%2)))
  names(computers.entries.criticity.rand) <- c("computer","criticidad")
  
  return(computers.entries.criticity.rand)
  
}

