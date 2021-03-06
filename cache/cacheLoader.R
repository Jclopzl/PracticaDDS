library("tidyr")

loadCacheMatchingCPE <- function(root.dir= getwd()) {

  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'matchingCPE.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
     
  return(matchingCPE.ds.cache)
  
}

loadCacheMatchingCVE <- function(root.dir= getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'matchingCVE.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(matchingCVE.ds.cache)
  
}

loadCacheLookupCPEByApplicaton <- function(root.dir=getwd()) {

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

loadCacheScoring <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'scoring.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(scoring.ds.cache)
  
}

buildAndSaveCacheMatchingCPE <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')

  first <- TRUE
  for (file in list.files(input.dir, pattern="matchingCPE.ds.worker[0-9][0-9]*.RData",include.dirs = FALSE,full.names = TRUE) ) {
    load(file)
    if( first ) {
      first=FALSE
      matchingCPE.ds.all <-  matchingCPE.ds
    } else {
      matchingCPE.ds.all <- dplyr::bind_rows(list(matchingCPE.ds.all,matchingCPE.ds)) 
    }
  }

  matchingCPE.ds.cache <- matchingCPE.ds.all
  save(matchingCPE.ds.cache,file = paste( input.dir,'matchingCPE.ds.cache.RData',sep="/"))

}

loadCacheMatchingCVE <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'matchingCVE.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(matchingCVE.ds.cache)
  
}