loadCacheLookupCPEByApplicaton <-
function(root.dir=getwd()) {

  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'lookupCPEByApplication.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
    
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(lookupCPEByApplication.ds.cache)

}
