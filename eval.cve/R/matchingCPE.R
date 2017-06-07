matchingCPE <-
function(applications.name, listado.cpe) { 
  
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
