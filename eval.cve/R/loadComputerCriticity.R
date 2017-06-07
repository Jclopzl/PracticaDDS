loadComputerCriticity <-
function(input.file, computer.entries=NULL, random=FALSE) {
  
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
