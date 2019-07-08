
standardise_USUBJID <- function(STUDYID, USUBJID) {
  
  # standardize USUBJID
  CLID = USUBJID 
  study.id = unique(STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  
  for (i in 1:max(1,length(study.id))) { 
    CLID = gsub(study.id[i], "", CLID, fix=TRUE)
  }
  CLID = gsub("-", "", CLID, fix=TRUE)   
  
  t1 = unique(nchar(CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)==0)  {print("warning: no USUBJID")
  }else if (length(t1)>1)  {print("warning: the length of CLID in nmdat are not the same.")
  }else{ 
    if (t1==9) { SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-")}
    if (t1==6) { SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-")}
    if (t1==3) { SUBJECT=paste(substr(CLID, 1, 3), sep="-")}
    if (!t1 %in% c(3, 6, 9)) {print("warning: nchar(CLID) in nmdat !=3, 6 or 9")}
    USUBJID <- paste(STUDYID,  SUBJECT, sep="-") 
    
  }
  return(USUBJID)
}
