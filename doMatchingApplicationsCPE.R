doMatchingApplicationsCPE <- function(computers.entries,cves) {
  
  # Generate lookup in order to make a relation or lookup table between software name and official CPE
  lookupCPEByApplication.ds <- lookupCPEByApplication(computers.entries$name)
  listado.cpe <- cpes
  matchingCPE.ds <- matchingCPE(lookupCPEByApplication.ds$name, listado.cpe)
  matchingCPE.ds <- as.data.frame.array(t(t(matchingCPE.ds)))
  names(matchingCPE.ds) <- c("cpe")
  lookupCPEByApplication.ds <- bind_cols( lookupCPEByApplication.ds, matchingCPE.ds)
  
  # Do matching between application names and CPEs names
  computers.entries.cves <- left_join(computers.entries,lookupCPEByApplication.ds,by="name")
  
  return(computers.entries.cves)
  
}