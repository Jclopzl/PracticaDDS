#install.packages("ngram")
#install.packages("stringdist")
#install.packages("dplyr")
library("ngram")
library("stringdist")
library("dplyr")

# Clear environment
rm(list=ls())

rootDir <- "~/workspace/gitrepos/repos/PracticaDDS"

# Load scripts
source(paste(rootDir,"input/dataloader.R",sep="/"))
source(paste(rootDir,"comparar.R",sep="/"))
source(paste(rootDir,"matchingCPE.R",sep="/"))
source(paste(rootDir,"lookupCPEByApplication.R",sep="/"))
source(paste(rootDir,"doMatchingApplicationsCPE.R",sep = "/"))
source(paste(rootDir,"calculateScoring.R",sep = "/"))


INPUT_COMPUTER_DIR <- paste(rootDir,"samples/input/computers/all",sep="/")
INPUT_SECURITY_DIR <- "input"

#0 Load official cve/cpes



if(TRUE) {
##downloadSysdata(INPUT_SECURITY_DIR)
load("~/workspace/gitrepos/repos/PracticaDDS/input/sysdata.rda")

# Load all CVEs/CPEs. Take care of that, a lot of data is loaded
#cves <- netsec.data$datasets$cves
cpes <- netsec.data$datasets$cpes

# Only load a sample from all CVEs/CPEs.
#cpes <- net.security::GetDataFrame("cpes") 
cves <- net.security::GetDataFrame("cves")

cpes <- head(cpes,n=60)

#1 Collect data from PCs
computers.entries <- loadComputerSoftware(INPUT_COMPUTER_DIR)


computers.entries.criticity <- loadComputerCriticity(computers.entries)

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

