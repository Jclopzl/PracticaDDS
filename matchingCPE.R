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

# Returns TRUE if the CPE matches with incoming application name
# Example: getMatchingCPEListWords( "windows", ngram::get.phrasetable(  ngram(c("microsoft cad windows"),1,sep=" ")  ))
getMatchingCPEListWords <- function(cpe.title, incoming.name.listofwords) {
  
  cpe.name.ng <- ngram(cpe.title,1,sep = " ")
  cpe.name.ng.listofwords <- ngram::get.phrasetable(cpe.name.ng)
  

  # Get matching taking into an account "distance" between words
  return( stringdist::ain(incoming.name.listofwords$ngrams,cpe.name.ng.listofwords$ngrams) )
  
}
out <- getMatchingCPEListWords( "windows", ngram::get.phrasetable(  ngram(c("microsoft cad windows"),1,sep=" ")  ))



# res <- getMatchingCPEListWords( "windows xp", ngram::get.phrasetable(  ngram(c("microsoft cad xp csdadsa windows"),1,sep=" ")  ))

# application.name <- tolower("Firephp")
# listado.cpe <- cpes
# listado.cpe$title <- tolower(listado.cpe$title)
# res <- getMatchingCPEApplication(application.name,listado.cpe)



