loadScoringTesting <-
function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,"samples/input/visualization",'cache',sep='/')
  file.name <- 'scoring_test.csv'
  file.fullname <- paste( input.dir,file.name,sep="/")
  computers.entries.scoring <- read.table(file.name,header=T,sep=";")
  return(computers.entries.scoring)

}
