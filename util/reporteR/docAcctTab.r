


   
#' Creates a Concentration vs Time graph
#'
#' Creates a Concentration vs Time graph using Regeneron data.
#'
#' @param data A data frame with which to create the plots.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @param drug Drug name
#' @param log Should the log axis be used?
#' @param adjust Should an adjustment be applied to the errorbars?
#' @return A ggplot graphic.
#' @import gridExtra
#' @export
#' @examples
#'   inFile <- system.file("extdata/pk_pd_ada_all3.sas7bdat", package = "pkGraph")
#'   library(haven)
#'   theData <- read_sas(inFile)
#'   theData <- dataCheck(theData)
#'   concTime(data = theData)
#'   concTime(data = theData, adjust = TRUE)
docAcctTab <- function(FIGURE_ALL, TABLE_ALL, mydoc, myppt, PKreport,             
  font.size=12, errorbar = "SD" 
 ) { 

#debug
if (1==2) {
FIGURE_ALL = RESULT$FIGURE_ALL
TABLE_ALL = RESULT$TABLE_ALL
font.size=12
errorbar = "SD" 
}

################################################################################
################################################################################
# Extract meta information
################################################################################
################################################################################

# ids.not.na <- which(!is.na(suppressWarnings(as.numeric(as.vector(tmp)))))    
txt=names(FIGURE_ALL)
txt=paste(txt,'=',"FIGURE_ALL$",txt,";",sep="")
eval(parse(text=txt))

txt=names(TABLE_ALL)
txt=paste(txt,'=',"TABLE_ALL$",txt,";",sep="")
eval(parse(text=txt))

#lib_DRUG <- convert.info(PKreport, type="DRUG")
#lib_TEST <- convert.info(PKreport, type="TEST")
#lib_STUDY <- convert.info(PKreport, type="STUDY")

lib_AUTHOR <- PKreport$lib_AUTHOR 
lib_TEST  <- PKreport$lib_TEST 
lib_STUDY <- PKreport$lib_STUDY 


drug_name =   as.character(lib_TEST["drug_name", "DESCRIPTIVE_NAME"])
study_name =  as.character(lib_STUDY["study_name", "DESCRIPTIVE_NAME"])


################################################################################
################################################################################
# Replace Table
################################################################################
################################################################################

#--------------------------------------------
# tab.title.std.errbar
#--------------------------------------------
tab.title.acct <- function(category, type, study_name) {
  if (type=="pk")  {type = "Pharmacokinetic Analysis"}
  if (type=="pd")  {type = "Pharmacodynamic Analysis"}
    
  tabl.title = paste("Accounting of ", str_to_title(category), " by ", type, " by Treatment Group and Overall (", study_name, ")", sep="") 
   return(tabl.title)
}
  
   
#--------------------------------------------
#  tab.footnote
#--------------------------------------------
tab.footnote <- function() {
  footnote = "HR = Hour(s); QW = Once a week; Q2W = Once every two weeks; SC = Subcutaneous; N = Number of patients; SD = Standard deviation; SE = Standard error"
  return(footnote)
}



#--------------------------------------------
# final write2doc
#--------------------------------------------
#tab_errbar_pk_sc = tab_errbar_pk[grep("SC", tab_errbar_pk$ARMA), ]
#tab_errbar_pk_iv = tab_errbar_pk[grep("IV", tab_errbar_pk$ARMA), ]
   
test.lst = as.character(setdiff(lib_TEST[, "ID"], c("drug_name", "drug_name_alias")))
tab.lst = paste("tab_errbar_", gsub("_test_name", "", test.lst, fix=TRUE), sep="")
tab.lst = intersect(tab.lst,  sort(list_bookmarks(mydoc)))
 
tab.lst = expand.grid( c("subject", "measurement"), c("pk", "pd"))
colnames(tab.lst) = c("category", "type") 
tab.lst$tab.name = paste("tab_accounting_", tab.lst$category, "_", tab.lst$type, sep="")
tab.lst


for (i in 1:nrow(tab.lst)) {
    tdata = eval(parse(text=tab.lst[i, "tab.name"]))
    if (all(is.na(tdata))) {next}
    
    if (nrow(tdata)>0) {
    tt <- tab.title.acct(category=as.character(tab.lst[i, "category"]),  
                         type=as.character(tab.lst[i, "type"]), 
                         study_name=as.character(lib_STUDY["study_name",  "DESCRIPTIVE_NAME"]) 
                        )
    mydoc <- addParagraph(mydoc, value=tt, bookmark=paste(tab.lst[i, "tab.name"], "_title", sep=""),stylename = "Normal")
    mydoc <- addFlexTable(mydoc,  vanilla.table(tdata), bookmark=tab.lst[i, "tab.name"] )
    mydoc <- addParagraph(mydoc, value=tab.footnote(),  bookmark=paste(tab.lst[i, "tab.name"], "_footnote", sep=""), stylename = "C-Footnote")
    }
}
  
  
return(list(mydoc=mydoc, myppt=myppt)) 
  
}

 