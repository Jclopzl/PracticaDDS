doMatchingApplicationsCVE <-
function(computers.entries.cpes, nist.entries.cves) {

  
  nist.entries.cves.cpes.pair <- apply(nist.entries.cves,1,function(cve){
  
      # Retrieeve current string of CPEs and split this string as a list ot CPE strings
      nist.entries.cves.ds=data.frame(as.list(cve['cpe.software']),stringsAsFactors = FALSE)
      nist.entries.cves.ds.cpes <- strsplit(nist.entries.cves.ds$cpe.software[[1]], split = ",")
      
      # Retrieve current cve identifier
      nist.entries.cve=cve['cve']
      
      # Do a clean process for each cpe
      nist.entries.cves.ds.cpes.cleaned <- lapply(nist.entries.cves.ds.cpes[[1]],function(nist.entries.cves.ds.cpe) {     
        
        
        valTemp <- nist.entries.cves.ds.cpe
        valTemp <- gsub(pattern = " ",replacement = "",x = nist.entries.cves.ds.cpe)
        valTemp <- gsub(pattern = "/a",replacement = "a",x = valTemp)
        valTemp <- gsub(pattern = "\\[",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "\\]",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = '"',replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "\\\\",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "/",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "\\{",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "\\}",replacement = "",x = valTemp)
        valTemp <- gsub(pattern = "%2",replacement = ".",x = valTemp)
        
        return(paste(nist.entries.cve,valTemp,sep='|'))
       
      })
  
      # For each CPE return a pair of values: CVE and CPE separate by "|". Example: CVE|CPE
      return(nist.entries.cves.ds.cpes.cleaned)
      
    })
    
  # Convert to a dataframe
  nist.entries.cves.cpes.pair.ds <- t(data.frame(nist.entries.cves.cpes.pair))
  nist.entries.cves.cpes.pair.ds <- data.frame(nist.entries.cves.cpes.pair.ds,stringsAsFactors = FALSE)
  names(nist.entries.cves.cpes.pair.ds) <- c("cve")
  
  # Separate into two columns: cve and cpe found
  nist.entries.cves.cpes.pair.ds <- nist.entries.cves.cpes.pair.ds %>% tidyr::separate(cve,into=c("cve","cpe"),sep = "\\|")
  
  # Join with current computers applications to add cve identifier
  computers.entries.cves <- left_join(computers.entries.cpes, nist.entries.cves.cpes.pair.ds, by=("cpe"))
  
  # Computer entries with CVE
  computers.entries.cves.nona <- filter(computers.entries.cves,!is.na(cve))
  
  # Computers entries without any CVE
  computers.entries.cves.na <- filter(computers.entries.cves,is.na(cve))
  
  # Add cvss information for CVEs found
  computers.entries.cves.nona.complete <- left_join(computers.entries.cves.nona, cves, by=("cve"))
  
  # Select required columns
  computers.entries.cves.nona.final <- select(computers.entries.cves.nona.complete, name,version,vendor,computer,cve,cvss)
  
  # Retrieve cvss value from json string
  computers.entries.cves.nona.final$cvss <- sapply(computers.entries.cves.nona.final$cvss, function(cvss.json) { 
    value <- strsplit(strsplit(strsplit(cvss.json,split="score")[[1]][2],split="\\]")[[1]][1],split="\\[")[[1]][2]
    value <- gsub(pattern = '"',replacement = "",x = value)
    return(as.numeric(value)) })
  
  # Complete computers with no CVE
  computers.entries.cves.na$cve=replicate(nrow(computers.entries.cves.na),NA)
  computers.entries.cves.na$cvss=replicate(nrow(computers.entries.cves.na),0)
  computers.entries.cves.na.final <- select(computers.entries.cves.na, name,version,vendor,computer,cve,cvss)
  
  # Join computers with cves and computers with no cvs
  return(dplyr::bind_rows(computers.entries.cves.na.final,computers.entries.cves.nona.final))
}
