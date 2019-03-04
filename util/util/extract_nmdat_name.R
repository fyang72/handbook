



extract_nmdat_name <- function(ctlModel) {  
  #ctlModel= readLines("./model/ctl/LN001.ctl")
  #ctlModel = "$DATA  ../../data/nmdat4LN001.csv  IGNORE=C  ;l llll==;;;sfsf"
  
  # find the row of $DATA, and remove any comments
  tt = unlist(strsplit(ctlModel[which(regexpr('$DATA', ctlModel, fix=TRUE)>0)], " "))
  tt = tt[which(tt!="")]
  ids = which(regexpr(';', tt, fix=TRUE)>0)
  if (length(ids)>0) {tt =  tt[1:(ids[1]-1)]}
  tt
  
  # remove any key words, such as $DATA, IGNORE=C, etc.
  ids = c(which(regexpr('$DATA', tt, fix=TRUE)>0),
          which(regexpr('IGNORE', tt, fix=TRUE)>0)
  )
  tt = tt[setdiff(1:length(tt), ids)]
          
  #library(stringr)
  #stringr::str_count(tt, fixed("../"))
  return(tt)
}



