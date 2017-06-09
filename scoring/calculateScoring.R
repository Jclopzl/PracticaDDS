# Calculate an scoring for each computer+
#
# @param computers.entries.cves computer information with cves for each PC
# @param computers.entries.criticity Criticity factor for each PC, depnding on their importance for the business.
# @param cache Load precalculated scoring according original source
# @param use_simulation true if use a sample file used to test visualization
calculateScoring <- function(computers.entries.cves, computers.entries.criticity, cache=FALSE,use_simulation=FALSE) {

  
  if( cache ) {
    
    computers.entries.scoring <- loadCacheScoring()
  
  } else if(use_simulation) {
    
    computers.entries.scoring <- loadScoringTesting()
    
  } else {
    
    computers.entries.scoring <- computers.entries.cves
    
    # Get the most cvss score
    computers.entries.scoring <- computers.entries.cves %>% group_by(computer) %>% arrange(desc(cvss)) %>% slice(1) %>% ungroup
    
    # Join criticity
    computers.entries.scoring <- left_join(computers.entries.scoring,computers.entries.criticity,by="computer")
    
    #names(computers.entries.cves) <- c("name","version","vendor","comuper","cve","cvss","criticidad")
    
    # Add default cvsse as cvss  
    computers.entries.scoring$cvsse <- sapply(computers.entries.scoring$cvss, function(cvss) return(cvss) )
    computers.entries.scoring$cvsse <- as.numeric(computers.entries.scoring$cvsse)
    
    #for( j in 1:nrow(computers.entries.scoring) ) {
    #  if( computers.entries.scoring['criticidad'][j] > 0) {
    #    computers.entries.scoring['cvsse'][j] <-  computers.entries.scoring['cvsse'][j] + 2
    #  }
    #  j <- j+1
    #}
    
    # Set critical cvss
    for( j in 1:nrow(computers.entries.scoring) ) {
      if( computers.entries.scoring['criticidad'][[1]][j] > 0) {
        computers.entries.scoring['cvsse'][[1]][j] <-  computers.entries.scoring['cvsse'][[1]][j] + 2
      }
      j <- j+1
    }
    computers.entries.scoring$nivel <- sapply(computers.entries.scoring$cvss, function(cvss) setNivel(cvss) )
    
    # Set criticity level
    computers.entries.scoring$nivele <- sapply(computers.entries.scoring$cvsse, function(cvss) setNivel(cvss) )  
    
  }
  
 
  return(computers.entries.scoring)
}
  
  


# Set level taking into an account the cvss
setNivel <- function(cvss) {
  if( cvss > 9 ) {
    
    return ("NIVEL_CRITICO")
    
  } else if ( cvss > 4 ) {
    
    return ("NIVEL_MEDIO")
    
  } else if ( cvss > 0 ) {
    
    return ("NIVEL_BAJO") 
    
  } else {
    
    return ("NO_VULNERABLE")
    
  }
}