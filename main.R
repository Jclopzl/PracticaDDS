install.packages("ngram")
install.packages("stringdist")
library("ngram")
library("stringdist")

rootDir <- "~/workspace/gitrepos/repos/PracticaDDS"

# Load scripts
source(paste(rootDir,"input/dataloader.R",sep="/"))
source(paste(rootDir,"comparar.R",sep="/"))

INPUT_COMPUTER_DIR <- paste(rootDir,"samples/input/computers",sep="/")
INPUT_SECURITY_DIR <- "input"

computers.entries <- loadComputerSoftware(INPUT_COMPUTER_DIR)

downloadSysdata(INPUT_SECURITY_DIR)
load("~/workspace/gitrepos/repos/PracticaDDS/input/sysdata.rda")

#Valores reales a usar
#cves <- netsec.data$datasets$cves
#cpes <- netsec.data$datasets$cpes
#Solo obtener 1000 valores para rapidez en calculos (solo de prueba)
cpes <- net.security::GetDataFrame("cpes") 
cves <- net.security::GetDataFrame("cves")

#test de funcion en datos a comparar
test <- lapply(computers.entries$Name, function(x) comparar(x, cpes$title))

