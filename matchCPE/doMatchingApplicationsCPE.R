library("dplyr")
library("tidyr")


doMatchingApplicationsCPE <- function(
  computers.entries=NULL,
  listado.cpe=cpes,
  threshold=0.5,
  cache=FALSE,
  distributed=FALSE,
  root.dir=getwd(),
  applications.name.limit=NULL, 
  listado.cpe.limit=NULL
) {

  # Initialize working dataframes
  lookupCPEByApplication.ds <- NULL
  matchingCPE.ds <- NULL
  
  # If use cache, load cached objects
  if( cache ) {
    lookupCPEByApplication.ds <- loadCacheLookupCPEByApplicaton(rootDir)
    matchingCPE.ds <- loadCacheMatchingCPE(root.dir)
    
    # Matching also have lookupCEPByApplication data, so replace it
    if( !is.null( matchingCPE.ds) ) {
      lookupCPEByApplication.ds <- matchingCPE.ds
    }
  }
  
  # Check if it is required to calculate lookupCPEByApplication.ds dataframe
  if( is.null(matchingCPE.ds) & is.null(lookupCPEByApplication.ds) ) {
    # Generate lookup in order to make a relation or lookup table between software name and official CPE
    lookupCPEByApplication.ds <- lookupCPEByApplication(computers.entries$name)
  }
  
  # Check if it is required to calculate matchingCPE.ds dataframe
  if(is.null(matchingCPE.ds)) {

    listado.cpe <- cpes
    
    # Reduce data frames for testing if required
    if( !is.null(applications.name.limit) ) {
      lookupCPEByApplication.ds <- head(lookupCPEByApplication.ds,n=applications.name.limit)
    }
    
    # Reduce data frames for testing if required
    if( !is.null(listado.cpe.limit) ) {
      listado.cpe <- head(listado.cpe,n=listado.cpe.limit)
    }
    
    # Only to split work into many workers
    if( distributed & ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
      worker.all <- as.numeric(Sys.getenv("worker_all"))
      worker.current <- as.numeric(Sys.getenv("worker_current"))
      df.total <- nrow(lookupCPEByApplication.ds)
      df.group.size <- ceiling(df.total/worker.all) 
      df.group.list <- rep(1:ceiling(df.total/df.group.size),each=df.group.size)[1:df.total]
      df.splitted <- split(lookupCPEByApplication.ds,df.group.list)
      lookupCPEByApplication.ds <- df.splitted[[worker.current]]
    }
    
    # Convert to data frame
    matchingCPE.ds <- matchingCPE(lookupCPEByApplication.ds$name, listado.cpe); 
    matchingCPE.ds <- as.data.frame.array(t(t(matchingCPE.ds)))
    names(matchingCPE.ds) <- c("cpe")

    
    # Add matching to current lookup application list
    lookupCPEByApplication.ds <- bind_cols( lookupCPEByApplication.ds, matchingCPE.ds)
    lookupCPEByApplication.ds <- lookupCPEByApplication.ds %>% separate(cpe,c("cpe","coef","poscpe"),sep="\\|")
    
  }
  
    
  # Filter coef column taking into an account the threshols value
  lookupCPEByApplication.ds$coef <- as.numeric(lookupCPEByApplication.ds$coef)
  
  names(matchingCPE.ds)
  names(lookupCPEByApplication.ds)
  
  lookupCPEByApplication.ds <-lookupCPEByApplication.ds %>% mutate(cpe = ifelse(coef< threshold,NA,cpe))
  
  lookupCPEByApplication.ds <- subset(lookupCPEByApplication.ds, select=-c(coef,poscpe))
  
  # Do matching between application names and CPEs names
  computers.entries.cves <- left_join(computers.entries,lookupCPEByApplication.ds,by="name")
  
  return(computers.entries.cves)  

}
