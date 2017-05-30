library("dplyr")

# Get all software from all computers, get unique software names and get CPE id
# 
# @param applications.name dataset with all applications
# Example: res <- lookupCPEByApplication(computers.entries$Name)
lookupCPEByApplication <- function(applications.name) {

  #applications.name <- computers.entries$Name
  applications.name.ds <- as.data.frame.array(x=t(t(applications.name)))
  names(applications.name.ds) <- c("name")
  out <- applications.name.ds.distinct <- applications.name.ds %>% group_by(name) %>% summarise()
  out$name <- as.character(out$name)
  return (out)

}