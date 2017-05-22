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
    computer.entries <- read.csv(paste(inputDir,file,sep="/"),header=TRUE,sep=";")
    computer.entries$computer <- rep(file,nrow(computer.entries))
    
    if (exists('all.computers.entries')) {
      
      all.computers.entries <- dplyr::union(all.computers.entries,computer.entries)
    } else {
      all.computers.entries <- computer.entries
    }
  }
  
  return(all.computers.entries)

}

