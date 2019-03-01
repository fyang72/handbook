u.find.first.occurrence <- function(list.of.string, symbol="-", first=1, perl = FALSE){
  
  return(unlist(lapply(gregexpr(symbol, list.of.string, fixed=TRUE, perl = perl)
                       , function(x) cbind(x, attr(x, "match.length"))[first,1]-1)) )}