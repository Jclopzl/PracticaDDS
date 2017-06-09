library("tidyr")

# Load software inventory from all computers files included in the input.dir path
# 
# @description Read all files contained into <input.dir>. Each file is a CSV file. The file format is:
#   First line: contains header
#   Column 1: Name. Software or application name
#   Column 2: Version. Software versión
#   Column 3: Vendor. Company who has developed the product.
#
# The name of the file is taken as The name of the computer.
#
# @example: loadComputerSotware input/computers
# @param input.dir Path containing software computer input files
loadComputerSoftware <- function(input.dir) {
  
  
  
  for (file in list.files(input.dir)) {
    computer.entries <- read.csv(paste(input.dir,file,sep="/"),header=TRUE,sep=";",stringsAsFactors = FALSE)
    computer.entries$computer <- rep(file,nrow(computer.entries))
    
    if (exists('all.computers.entries')) {
      all.computers.entries <- dplyr::union(all.computers.entries,computer.entries)
    } else {
      all.computers.entries <- computer.entries
    }
  }
  
  names(all.computers.entries) <- c("name","version","vendor","computer") 
  
  return(all.computers.entries)

}

# Download CPE/CVE data from github
#
# @param input.dir destination directory to put the downloaded file relative to current workspace home location
# @example downloadSysdata input
downloadSysdata <- function(input.dir) {
  
  url <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  path <- paste(input.dir,"sysdata.rda",sep = "/")
  download.file(url = url, path)
  
}


# Load files containing the relation beetween the computer and the company criticity 
#
# @param input.file CSV file containing the information, The format of the file is
#         First line: header containing the titles.
#         Column1: computer name
#         Column2: Criticity. Number: 0 no critial, 1 critical
# @param random  to generate an artificial input
# @param computer.entries in case of random=TRUE, the list of computers. It takes computers names from this dataset.
loadComputerCriticity <- function(input.file, computer.entries=NULL, random=FALSE) {
  
  computers.entries.criticity <- as.data.frame(x=c())
  
  if( random && !is.null(computers.entries)) {
    # Generación aleatoria de criticidad
    computers.entries.criticity <- dplyr::select(computers.entries,computer) %>% distinct() 
    computers.entries.criticity$criticidad <- as.data.frame(sample(x=rep(x=1:nrow(computers.entries.criticity)%%2)))
  } else {
    computers.entries.criticity <- read.csv(input.file,header=TRUE,sep=";",stringsAsFactors = FALSE)
  }
 
  names(computers.entries.criticity) <- c("computer","criticidad")
  
  return(computers.entries.criticity)
  
}

# Load simulation data for testing visualization phase
#
# @param root.dir project root dir to find CSV
#
loadScoringTesting <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,"samples/input/visualization",'cache',sep='/')
  file.name <- 'scoring_test.csv'
  file.fullname <- paste( input.dir,file.name,sep="/")
  computers.entries.scoring <- read.table(file.name,header=T,sep=";")
  return(computers.entries.scoring)

}