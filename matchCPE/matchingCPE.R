# Get matching for a list of applications name and a list/dayaset of CPEs
# Example: 
# applications.name <- c("Firephp")
# listado.cpe <- cpes
# matchingCPEs <- matchingCPE(applications.name, listado.cpe)
# applications.name$cpes <- matchingCPEs
matchingCPE <- function(applications.name, listado.cpe) { 
  
  matchingCPE <- NULL
  
  # Convert factor to string
  listado.cpe <- data.frame(lapply(listado.cpe, as.character), stringsAsFactors=FALSE)
  
  # Convert to lower case cpes and computers
  applications.name <- tolower(applications.name)
  listado.cpe$title <- tolower(listado.cpe$title)

  # Jaccard method works with sets
  
  # Get sets for CPEs names
  listado.cpe.listofwords <- lapply( 
    listado.cpe$title, 
    function(cpe.name) {  return( sets::as.set( strsplit(cpe.name," ")[[1]])) } ) 
  
  # Get sets for computers name
  applications.name.listofwords <- lapply( 
    applications.name, 
    function(application.name) {  return( sets::as.set( strsplit(application.name," ")[[1]])) } )
  
  # Get the matching similiarity values
  matchingCPE <- getMatchingCPEApplication(applications.name, listado.cpe, applications.name.listofwords, listado.cpe.listofwords); 
  
  # Enrich the similiarity values with a builded CPE id that will be used in CVE matching     
  matchingCPE <- sapply(matchingCPE, function(coefpos){
        coefpos.array <- strsplit(coefpos,split = "\\|")
        pos <- coefpos.array[[1]][2]
        
        # String with CEP id used in CVE matching
        matching.name.current <- paste("cpe",listado.cpe[pos,]['part'],listado.cpe[pos,]['vendor'],listado.cpe[pos,]['product'],listado.cpe[pos,]['version'],sep=":")
        
        # Enrich data adding CPE id and COEF and POS of CVE
        return(paste(matching.name.current,coefpos,sep = "|"));
  })
   
  return(matchingCPE)
  
}




# Get Matching for an application and a list of CPEs using jaccard method
# Note: Assuming both listado.cpe and application.name are in lower case
# Example:
#   application.name <- tolower("Firephp")
#   listado.cpe <- cpes
#   listado.cpe$title <- tolower(listado.cpe$title)
#   getMatchingCPEApplication(application.name,listado.cpe)
getMatchingCPEApplication <- function(applications.name, listado.cpe, applications.name.listofwords, listado.cpe.listofwords) {

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
  
