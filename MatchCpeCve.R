cves.sample <- tail(head(cves,n=3),n=10)


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
    valTemp <- cleanCpe(valTemp)
    
    
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

res <- left_join(computers.entries.cpes, cves.sample.prejoin.ds, by=("cpe"))