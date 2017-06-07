loadComputerSoftware <-
function(input.dir) {
  
  
  
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
