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
source(paste(rootDir,"matchCVE/doMatchingApplicationsCVE.R",sep = "/"))
source(paste(rootDir,"scoring/calculateScoring.R",sep = "/"))


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

# 3 Matching computer applications with official CPE
computers.entries.cpes <- doMatchingApplicationsCPE(
  computers.entries=computers.entries,
  listado.cpe=cpes,
  threshold=0.5,
  cache=TRUE,
  distributed=FALSE,
  applications.name.limit=0, 
  listado.cpe.limit=0
) 

# 4 Mathicng computer CPEs with official CVEs
computers.entries.cves <- doMatchingApplicationsCVE(computers.entries.cpes, cves, cache=TRUE )

# 5 Evaluate scoring
computers.entries.scoring <- calculateScoring (computers.entries.cves, computers.entries.criticity, cache=TRUE)
