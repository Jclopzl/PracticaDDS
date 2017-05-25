comparar <- function(nombreSoftware, nombreCpe){
  computers.name <- tolower(nombreSoftware)
  cpe.name <- tolower(nombreCpe)
  computers.name.ng <- ngram(computers.name,1,sep = " ")
  cpe.name.ng <- ngram(cpe.name,1,sep = " ")
  listofwords.computers.name <- ngram::get.phrasetable(computers.name.ng)
  listofwords.cpe.name <- ngram::get.phrasetable(cpe.name.ng)
  comparacion <- stringdist::ain(listofwords.computers.name$ngrams,listofwords.cpe.name$ngrams)
  j <- 0
  for(i in 1:length(comparacion)){
    if(isTRUE(comparacion[i])){
      j <- j+1
    }
    
  }
  if(j == length(comparacion)){
    return(listofwords.computers.name$ngrams)
  }else{
    return(FALSE)
  }
  
}