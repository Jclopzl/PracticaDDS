cves.sample <- tail(head(cves,n=30000),n=10000)

computers.entries.cpes <- matchingCPE.ds
cves.sample.prejoin <- apply(cves.sample,1,function(cve.sample){
  
  cve.sample.ds=data.frame(as.list(cve.sample['cpe.software']),stringsAsFactors = FALSE)
  cve.sample.ds.cpes<- strsplit(cve.sample.ds$cpe.software[[1]], split = ",")
  
  cve.sample.cve=cve.sample['cve']
  
  cve.sample.ds.cpes.cleaned <- lapply(cve.sample.ds.cpes[[1]],function(cve.sample.ds.cpe) {     
    
    
    valTemp <- cve.sample.ds.cpe
    valTemp <- gsub(pattern = " ",replacement = "",x = cve.sample.ds.cpe)
    valTemp <- gsub(pattern = "/a",replacement = "a",x = valTemp)
    valTemp <- gsub(pattern = "\\[",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "\\]",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = '"',replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "\\\\",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "/",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "\\{",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "\\}",replacement = "",x = valTemp)
    valTemp <- gsub(pattern = "%2",replacement = ".",x = valTemp)
    #valTemp <- cleanCpe(valTemp)
    
    
    
    return(paste(cve.sample.cve,valTemp,sep='|'))
    
    #    if( valTemp == "[" || valTemp == "]"){
    #    return('NA');
    #   } else{
    
    #   return (paste(valTemp,cve.sample.cpe[1]),sep="\\|")
    #  }
  })
  
  return(cve.sample.ds.cpes.cleaned)
  
})

# convert to a dataframe
cves.sample.prejoin.ds <- t(data.frame(cves.sample.prejoin))
cves.sample.prejoin.ds <- data.frame(cves.sample.prejoin.ds,stringsAsFactors = FALSE)

names(cves.sample.prejoin.ds) <- c("cve")

# Separate into two columns: cve and cpe found
cves.sample.prejoin.ds <- cves.sample.prejoin.ds %>% tidyr::separate(cve,into=c("cve","cpe"),sep = "\\|")

computers.entries.cves <- left_join(computers.entries.cpes, cves.sample.prejoin.ds, by=("cpe"))
computers.entries.cves.nona <- filter(computers.entries.cves,!is.na(cve))
computers.entries.cves.na <- filter(computers.entries.cves,is.na(cve))

# Add cvss information
computers.entries.cves.nona.complete <- left_join(computers.entries.cves.nona, cves, by=("cve"))

# Select required columns
computers.entries.cves.nona.final <- select(computers.entries.cves.nona.complete, name,version,vendor,computer,cve,cvss)

# Retrieve cvss value
computers.entries.cves.nona.final$cvss <- sapply(computers.entries.cves.nona.final$cvss, function(cvss.json) { 
  value <- strsplit(strsplit(strsplit(cvss.json,split="score")[[1]][2],split="\\]")[[1]][1],split="\\[")[[1]][2]
  value <- gsub(pattern = '"',replacement = "",x = value)
  return(as.numeric(value)) })

# Complete computers with no CVE
computers.entries.cves.na$cve=replicate(nrow(computers.entries.cves.na),NA)
computers.entries.cves.na$cvss=replicate(nrow(computers.entries.cves.na),0)
computers.entries.cves.na.final <- select(computers.entries.cves.na, name,version,vendor,computer,cve,cvss)

# Join computers with cves and computers with no cvs
computers.entries.cves.final <- dplyr::bind_rows(computers.entries.cves.na.final,computers.entries.cves.nona.final)


 
 
