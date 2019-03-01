


##############################################################################
#  functions 
##############################################################################  
#--------------------------------------------- 
#  nacols,  allna
#--------------------------------------------- 
nacols <- function(df) { 
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }
anyna <- function(df) {
  df = data.frame(df, check.names = FALSE)      
  t.df = data.frame(t(df), check.names = FALSE)
  return(df[nacols(t.df), nacols(df)])  }

