#install.packages("ngram")
#install.packages("stringdist")
#install.packages("dplyr")
library("ngram")
library("stringdist")
library("dplyr")

rootDir <- "~/workspace/gitrepos/repos/PracticaDDS"

# Load scripts
source(paste(rootDir,"input/dataloader.R",sep="/"))
source(paste(rootDir,"comparar.R",sep="/"))
source(paste(rootDir,"matchingCPE.R",sep="/"))
source(paste(rootDir,"lookupCPEByApplication.R",sep="/"))
source(paste(rootDir,"doMatchingApplicationsCPE.R",sep = "/"))

INPUT_COMPUTER_DIR <- paste(rootDir,"samples/input/computers/one",sep="/")
INPUT_SECURITY_DIR <- "input"

#0 Load official cve/cpes

##downloadSysdata(INPUT_SECURITY_DIR)
##load("~/workspace/gitrepos/repos/PracticaDDS/input/sysdata.rda")

# Load all CVEs/CPEs. Take care of that, a lot of data is loaded
#cves <- netsec.data$datasets$cves
cpes <- netsec.data$datasets$cpes

# Only load a sample from all CVEs/CPEs.
#cpes <- net.security::GetDataFrame("cpes") 
cves <- net.security::GetDataFrame("cves")

cpes <- head(cpes,n=50)

#1 Collect data from PCs
computers.entries <- loadComputerSoftware(INPUT_COMPUTER_DIR)
names(computers.entries) <- c("name","version","vendor","computer") 

# OPTIONAL: Work with a small sample
# computers.entries <- head(computers.entries,4) 

#2 Matching PC applications with CPE
computers.entries.cpes <- doMatchingApplicationsCPE(computers.entries,cpes)


