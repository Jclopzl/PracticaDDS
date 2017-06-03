library("dplyr")
library("tidyr")

doMatchingApplicationsCPE <- function(
  computers.entries=NULL,
  listado.cpe=cpes,
  jaccard=TRUE,
  lookupCPEByApplication.ds.cache.use=FALSE,
  lookupCPEByApplication.ds.cache.file=NULL,
  applications.name.limit=NULL, 
  listado.cpe.limit=NULL
) {

  # Generate lookup in order to make a relation or lookup table between software name and official CPE
  if( lookupCPEByApplication.ds.cache.use & ! is.null(lookupCPEByApplication.ds.cache.file) ) {
    load(lookupCPEByApplication.ds.cache.file)
    lookupCPEByApplication.ds <- lookupCPEByApplication.ds.cache
  } else {
    lookupCPEByApplication.ds <- lookupCPEByApplication(computers.entries$name)
  }

  listado.cpe <- cpes
  
  if( jaccard ) {
    
    # Reducing data frames for tests
    if( !is.null(applications.name.limit) ) {
      lookupCPEByApplication.ds <- head(lookupCPEByApplication.ds,n=applications.name.limit)
    }
    
    if( !is.null(listado.cpe.limit) ) {
      listado.cpe <- head(listado.cpe,n=listado.cpe.limit)
    }

    # Only to split work into many workers
    if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
      worker.all <- as.numeric(Sys.getenv("worker_all"))
      worker.current <- as.numeric(Sys.getenv("worker_current"))
      df.total <- nrow(lookupCPEByApplication.ds)
      df.group.size <- ceiling(df.total/worker.all) 
      df.group.list <- rep(1:ceiling(df.total/df.group.size),each=df.group.size)[1:df.total]
      df.splitted <- split(lookupCPEByApplication.ds,df.group.list)
      lookupCPEByApplication.ds <- df.splitted[[worker.current]]
    }
    
    # Convert to data frame
    matchingCPE.ds <- matchingCPEJaccard(lookupCPEByApplication.ds$name, listado.cpe, jaccard=TRUE); 
    matchingCPE.ds <- as.data.frame.array(t(t(matchingCPE.ds)))
    names(matchingCPE.ds) <- c("cpe")
    
    # Common
    # Add matching to current ookup application list
    lookupCPEByApplication.ds <- bind_cols( lookupCPEByApplication.ds, matchingCPE.ds)
    lookupCPEByApplication.ds <- lookupCPEByApplication.ds %>% separate(cpe,c("cpe","coef","poscpe"),sep="\\|")
    return(lookupCPEByApplication.ds)
    
  } else {
    
    matchingCPE.ds <- matchingCPE(lookupCPEByApplication.ds$name, listado.cpe)
    matchingCPE.ds <- as.data.frame.array(t(t(matchingCPE.ds)))
    names(matchingCPE.ds) <- c("cpe")
    lookupCPEByApplication.ds <- bind_cols( lookupCPEByApplication.ds, matchingCPE.ds)
    
    # Do matching between application names and CPEs names
    computers.entries.cves <- left_join(computers.entries,lookupCPEByApplication.ds,by="name")
    
    return(computers.entries.cves)  
  }
   
  
  
  
}
