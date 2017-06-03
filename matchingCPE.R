# Get matching for a list of applications name and a list/dayaset of CPEs
# Example: 
# applications.name <- c("Firephp")
# listado.cpe <- cpes
# matchingCPEs <- matchingCPE(applications.name, listado.cpe)
# applications.name$cpes <- matchingCPEs
matchingCPE <- function(applications.name, listado.cpe){

  # Convert factor to string
  listado.cpe <- data.frame(lapply(listado.cpe, as.character), stringsAsFactors=FALSE)
  
  # Convert to lower case cpes and computers
  applications.name <- tolower(applications.name)
  listado.cpe$title <- tolower(listado.cpe$title)

  # Optimize calculating once listofwords for cpe list
  listado.cpe$listofwords <- lapply( 
      listado.cpe$title, 
      function(cpe.name) { cpe.name.ng <- ngram(cpe.name,1,sep = " "); return(ngram::get.phrasetable(cpe.name.ng)) } ) 

  # For each application get their CPE
  matchingCPE <- lapply( applications.name, function(application.name) getMatchingCPEApplication(application.name,listado.cpe ) )

  return(matchingCPE);
  
}


# Get matching for a list of applications name and a list/dayaset of CPEs
# Example: 
# applications.name <- c("Firephp")
# listado.cpe <- cpes
# matchingCPEs <- matchingCPE(applications.name, listado.cpe)
# applications.name$cpes <- matchingCPEs
matchingCPEJaccard <- function(applications.name, listado.cpe, jaccard=TRUE) { 
  
  matchingCPE <- NULL
  
  # Convert factor to string
  listado.cpe <- data.frame(lapply(listado.cpe, as.character), stringsAsFactors=FALSE)
  
  
  
  # Convert to lower case cpes and computers
  applications.name <- tolower(applications.name)
  listado.cpe$title <- tolower(listado.cpe$title)

  if( jaccard ) {
    
    # Jaccard method works with sets
    
    # Get sets for CPEs names
    listado.cpe.listofwords <- lapply( 
      listado.cpe$title, 
      function(cpe.name) {  return( sets::as.set( strsplit(cpe.name," ")[[1]])) } ) 
    
    # Get sets for computers name
    applications.name.listofwords <- lapply( 
      applications.name, 
      function(application.name) {  return( sets::as.set( strsplit(application.name," ")[[1]])) } )
    
    # Get the matching
    matchingCPE.coef <- getMatchingCPEApplicationJaccard(applications.name, listado.cpe, applications.name.listofwords, listado.cpe.listofwords); 
          
        matchingCPE <- sapply(matchingCPE.coef, function(coefpos){
        coefpos.array <- strsplit(coefpos,split = "\\|")
        pos <- coefpos.array[[1]][2]
        matching.name.current <- paste("cpe",listado.cpe[pos,]['part'],listado.cpe[pos,]['vendor'],listado.cpe[pos,]['product'],listado.cpe[pos,]['version'],sep=":")
        return(paste(matching.name.current,coefpos,sep = "|"));
    })
     
    
    applications.name <-  matchingCPE.coef
    #matchingCPE <- applications.name
    
  }
  return(matchingCPE);
}




# Get Matching for an application and a list of CPEs using jaccard method
# Note: Assuming both listado.cpe and application.name are in lower case
# Example:
#   application.name <- tolower("Firephp")
#   listado.cpe <- cpes
#   listado.cpe$title <- tolower(listado.cpe$title)
#   getMatchingCPEApplication(application.name,listado.cpe)
getMatchingCPEApplicationJaccard <- function(applications.name, listado.cpe, applications.name.listofwords, listado.cpe.listofwords) {

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
  
























# Get Matching for an application and a list of CPEs
# Note: Assuming both listado.cpe and application.name are in lower case
# Example:
#   application.name <- tolower("Firephp")
#   listado.cpe <- cpes
#   listado.cpe$title <- tolower(listado.cpe$title)
#   getMatchingCPEApplication(application.name,listado.cpe)
getMatchingCPEApplication <- function(application.name, listado.cpe){
  
  application.name.listofwords <- ngram::get.phrasetable(ngram(application.name,1,sep=" "))
  
  cpe.colnames <- colnames(listado.cpe)
  
  # Foreach cpe compare nombreSoftware with nombreCpe
  if( "listofwords" %in%  cpe.colnames ) {
    
    matching_computer_cpe <- 
      lapply(
        listado.cpe$listofwords, function(cpe.name.ng.listofwords)  return( stringdist::ain(application.name.listofwords$ngrams,cpe.name.ng.listofwords$ngrams) ) )  
    
  } else {

      matching_computer_cpe <- 
        lapply(
          listado.cpe$title, function(cpe.title) getMatchingCPEListWords(cpe.title, application.name.listofwords))
  }
  

  # Search for max
  matchCPE.name <- NULL
  matchCPE.value <- 0
  
  
  j=1
  for(i in 1:length(matching_computer_cpe)){
    
    matching.words <- matching_computer_cpe[[i]]
    
    matching.name.current <- paste(
      "cpe",
      listado.cpe[j,]['part'],
      listado.cpe[j,]['vendor'],
      listado.cpe[j,]['product'],
      listado.cpe[j,]['version'],
      sep=":")
    
    matching.value.current <- sum(matching.words)
    if(  matching.value.current > matchCPE.value ) {
      print(matching.name.current)
      matchCPE.name <- matching.name.current
      matchCPE.value <- matching.value.current
    }
    
    j <- j+1
    
  }
  
  return(matchCPE.name);
  
}


