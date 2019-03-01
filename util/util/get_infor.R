
############################################################################## 
# get.infor
############################################################################## 

# from adpk[,"TIMEPT"] borrow "adpk$VISIT" to adlb$VISIT
# based on first available information , sinc we used "!duplicated()
#' Extracts the time matched concntration vs effect data

#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)  
get_infor <- function(adpk, adlb, col.lst=c("TIMEPT"), col.subs=c("VISIT")) {
  
  adpk = data.frame(adpk)
  adlb = data.frame(adlb)
  
  missing.col <- setdiff(col.lst, colnames(adpk))
  if (length(missing.col)>0) print(paste("No common column of ", missing.col, " in adpk", sep="")) 
  
  missing.col <- setdiff(col.lst, colnames(adlb))
  if (length(missing.col)>0) print(paste("No common column of ", missing.col, " in adlb", sep="")) 
  
  missing.col <- setdiff(col.subs, colnames(adpk))
  if (length(missing.col)>0) print(paste("No subs column of ", missing.col, " in adpk", sep="")) 
  
  adpk$TMP = pasteCol(adpk, col.lst)
  t1 = adpk[which(!duplicated(adpk$TMP)), ]
  rownames(t1) <- t1$TMP  
  
  return(t1[match(as.character(pasteCol(adlb, col.lst)), rownames(t1)), col.subs]) 
}



# from adpk[,"TIMEPT"] borrow "adpk$VISIT" to adlb$VISIT
# based on first available information , sinc we used "!duplicated()
#' Extracts the time matched concntration vs effect data

#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)  
get.infor <- function(adpk, adlb, col.lst=c("TIMEPT"), col.subs=c("VISIT")) {
  adpk = data.frame(adpk)
  adlb = data.frame(adlb)
  
  missing.col <- setdiff(col.lst, colnames(adpk))
  if (length(missing.col)>0) print(paste("No common column of ", missing.col, " in adpk", sep="")) 
  
  missing.col <- setdiff(col.lst, colnames(adlb))
  if (length(missing.col)>0) print(paste("No common column of ", missing.col, " in adlb", sep="")) 
  
  missing.col <- setdiff(col.subs, colnames(adpk))
  if (length(missing.col)>0) print(paste("No subs column of ", missing.col, " in adpk", sep="")) 
  
  adpk$TMP = pasteCol(adpk, col.lst)
  t1 = adpk[which(!duplicated(adpk$TMP)), ]
  rownames(t1) <- t1$TMP  
  
  return(t1[match(as.character(pasteCol(adlb, col.lst)), rownames(t1)), col.subs]) 
}



