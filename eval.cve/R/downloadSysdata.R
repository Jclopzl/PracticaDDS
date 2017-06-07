downloadSysdata <-
function(input.dir) {
  
  url <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  path <- paste(input.dir,"sysdata.rda",sep = "/")
  download.file(url = url, path)
  
}
