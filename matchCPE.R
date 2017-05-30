# Returns TRUE if the CPE matches with incoming application name
# Example: matchCPE( "windows", ngram::get.phrasetable(  ngram(c("microsoft casa casa casa casa windows"),1,sep=" ")  ))
matchCPE <- function(cpe.title, listofwords.incoming.name) {

  cpe.name.ng <- ngram(cpe.title,1,sep = " ")
  listofwords.cpe.name <- ngram::get.phrasetable(cpe.name.ng)

  # Get matching taking into an account "distance" between words
  return( stringdist::ain(listofwords.incoming.name$ngrams,listofwords.cpe.name$ngrams) )

  
}