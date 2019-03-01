

##############################################################################
# substitute a str1 with str2 in a str vector
##############################################################################
u.str.subs <- function(x, str1="", str2="") {  
  ids <- which(x==str1)
  x[ids] <- str2
  return(x)
} 

u.str.subs <- function(x, str1="", str2="") { 
  stopifnot(length(str1)==length(str2))
  
  for (i in 1:length(str1)) { 
    ids <- which(x==str1[i])
    x[ids] <- str2[i]
  }
  return(x)
} 


subs <- function(x, str1="", str2="") {  
  ids <- which(x==str1)
  x[ids] <- str2
  return(x)
} 

u.subs.symbol <- function(tt, symbol.from="~", symbol.to="-",first=2) {
  ids <- u.find.first.occurrence(tt, symbol.from, first, perl = FALSE)
  t1 <- substr(tt, 1, ids)
  t2 <- gsub(symbol.from, symbol.to, substr(tt, ids+1, ids+1) )
  t3 <- substr(tt, ids+2, nchar(tt))
  t0 <- paste(t1, t2, t3, sep="")
  
  return(t0)
  
}    
