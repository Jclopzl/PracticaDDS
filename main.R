# Utility package method
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x)
    require(x,character.only=TRUE)
  }
}

# packages("ngram")
# packages("stringdist")
# packages("tidyr")
# packages("dplyr")
#   Required on CentOS to install devtools: yum install libcurl-devel
#   install.packages("devtools")
# packages("devtools")
#   devtools::install_github(repo = "r-net-tools/net.security")
#   sudo yum install libxml2-devel required by XML package on CentOS 7
#   sudo yum install openssl-devel required by devtools/gitr package on CentOS 7
#   install.packages("XML") required by net.security
# packages("XML")
# packages("net.security")
# packages("sets")
library("devtools")
library("XML")
library("ngram")
library("stringdist")

# Do not load, %>% in dpylr and tidylr is masked by sets
#library("sets")
# detach("package:sets", unload=TRUE)
library("dplyr")
library("tidyr")



# Clear environment
rm(list=ls())

rootDir <- "~/workspace/gitrepos/repos/PracticaDDS2"

rootDir <- getwd()

# Load scripts
source(paste(rootDir,"input/inputLoader.R",sep="/"))
source(paste(rootDir,"cache/cacheLoader.R",sep="/"))
source(paste(rootDir,"matchCPE/matchingCPE.R",sep="/"))
source(paste(rootDir,"matchCPE/lookupCPEByApplication.R",sep="/"))
source(paste(rootDir,"matchCPE/doMatchingApplicationsCPE.R",sep = "/"))
source(paste(rootDir,"comparar.R",sep="/"))
source(paste(rootDir,"calculateScoring.R",sep = "/"))


INPUT_COMPUTER_DIR <- paste(rootDir,"samples/input/computers/all",sep="/")
INPUT_COMPUTER_CRITICITY_FILE <- paste(rootDir,"samples/input/computers/criticity.csv",sep="/")
INPUT_SECURITY_DIR <- "input"


# 1 Load official cve/cpes
#downloadSysdata(INPUT_SECURITY_DIR)
load("~/workspace/gitrepos/repos/PracticaDDS/input/sysdata.rda")

# All CVEs/CPEs. Take care of that, a lot of data is loaded
cves <- netsec.data$datasets$cves
cpes <- netsec.data$datasets$cpes

# Only load a sample from all CVEs/CPEs.
#cpes <- net.security::GetDataFrame("cpes") 
#cves <- net.security::GetDataFrame("cves")

# 2 Collect data from PCs
computers.entries <- loadComputerSoftware(INPUT_COMPUTER_DIR)
computers.entries.criticity <- loadComputerCriticity(INPUT_COMPUTER_CRITICITY_FILE);

# 3 Matching computer applications with CPE
matchingCPE.ds <- doMatchingApplicationsCPE(
  computers.entries=computers.entries,
  listado.cpe=cpes,
  threshold=0.5,
  cache=TRUE,
  distributed=FALSE,
  applications.name.limit=2, 
  listado.cpe.limit=50000
) 

if( FALSE ) {
  if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
    worker.all <- as.numeric(Sys.getenv("worker_all"))
    worker.current <- as.numeric(Sys.getenv("worker_current"))
    save(matchingCPE.ds,file=paste(rootDir,'cache/matchingCPE.ds.worker',worker.current,".RData",sep=""))
  }
}

if(FALSE) {

  

# OPTIONAL: Work with a small sample
# computers.entries <- head(computers.entries,4) 

#2 Matching PC applications with CPE
# computers.entries.cpes <- doMatchingApplicationsCPE(computers.entries,cpes)
}

if( FALSE ) {

# Evaluate the level of healthy

# Input test: computers.entries with cve
input=list(
          c("Git version 2.8.3","Adobe","Repe"),
          c("2.8.3","1.2","1.3"),
          c("The Git Development Community","adobe company","Otra company"),
          c("0001_bcn-84wpk32.csv","0001_bcn-84wpk32.csv","0001_bcn-84wpk32.csv"),
          c("cve-2000-2010","cve-3233-2111","cve-3233-2991"),
          c("5","2","5")
        )

computers.entries.cves <- as.data.frame.list(x=t(input),stringsAsFactors = FALSE)
names(computers.entries.cves) <- c("name","version","vendor","computer","cve","cvss")
print(computers.entries.cves)
computers.entries.cves$cvss <- computers.entries.cves$cvss
print(computers.entries.cves)

# Input test: criticiy
input=c("0001_bcn-84wpk32.csv","1")
#computers.entries.scoring$criticidad <- rep(1, nrow(computers.entries.scoring))
computers.entries.cricity<- as.data.frame(x=t(input), stringsAsFactors = FALSE)
names(computers.entries.cricity) <- c("computer","criticidad")
computers.entries.cricity$criticidad <- as.numeric(computers.entries.cricity$criticidad)
  
# Evaluate scoring
computers.entries.scoring <- calculateScoring (computers.entries.cves, computers.entries.cricity)
    
}

