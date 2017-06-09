getMatchingCPEApplication <-
function(applications.name, listado.cpe, applications.name.listofwords, listado.cpe.listofwords) {

  # Print current progress
  total <- length(applications.name)
  progress <- as.numeric(1)

  matchingCPE <- lapply( applications.name.listofwords, function(application.name) {

    timer.start <- Sys.time()
    
    # Array with jaccard result
    matchingCPE.jaccard.coef <-lapply(listado.cpe.listofwords, function(cpe.name) {
        val <- sets::set_similarity(cpe.name,application.name, method = 'Jaccard')
        return(val)
    })
  
    # Returns the CPE and the jaccard value for the highest jaccard value found. Format [CEP]:[Jaccard_coef]
    pos <- which.max(matchingCPE.jaccard.coef)
    timer.end <- Sys.time()
    timer.elapsed <- round(as.numeric(timer.end - timer.start)*60,digits=2) 
    print(paste(progress," of ",total," [",timer.elapsed,"]"))
   
    # <<- Need to acces out of curent function scope
    progress <<- progress + 1;
    return(paste(matchingCPE.jaccard.coef[[pos]],pos,sep="|"))
  
  })
  
  return(matchingCPE) 

}
