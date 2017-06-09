setNivel <-
function(cvss) {
  if( cvss > 9 ) {
    
    return ("NIVEL_CRITICO")
    
  } else if ( cvss > 4 ) {
    
    return ("NIVEL_MEDIO")
    
  } else if ( cvss > 0 ) {
    
    return ("NIVEL_BAJO") 
    
  } else {
    
    return ("NO_VULNERABLE")
    
  }
}
