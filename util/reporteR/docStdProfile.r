


   
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
docStdProfile <- function(FIGURE_ALL, TABLE_ALL, mydoc, myppt, regnR_PKreport,             
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
#
#lib_DRUG <- convert.info(regnR_PKreport, type="DRUG")
#lib_TEST <- convert.info(regnR_PKreport, type="TEST")
#lib_STUDY <- convert.info(regnR_PKreport, type="STUDY")
#
lib_AUTHOR <- regnR_PKreport$lib_AUTHOR 
lib_TEST  <- regnR_PKreport$lib_TEST 
lib_STUDY <- regnR_PKreport$lib_STUDY 

drug_name =   lib_TEST["drug_name", "DESCRIPTIVE_NAME"]
study_name =  lib_STUDY["study_name", "DESCRIPTIVE_NAME"]


################################################################################
################################################################################
# Replace Table
################################################################################
################################################################################

#--------------------------------------------
# tab.title.std.errbar
#--------------------------------------------
tab.title.std.errbar <- function(pk_name, drug_name, study_name, patient=NULL, arma=NULL) {
  tabl.title = paste("Summary of ", pk_name, " in Serum by Time Point Following Single/Multiple Intravenous/Subcutaneous Dose(s) of ", drug_name, " in Study ", study_name, "", sep="") 
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

if (length(tab.lst)==0) {warning("no specified table in Excel Sheet and Doc tempalte!")}
stopifnot(length(tab.lst)>0)
 
for (i in 1:length(tab.lst)) {
    tdata = eval(parse(text=tab.lst[i]))
   
    pk_name=lib_TEST[test.lst[i],  "DESCRIPTIVE_NAME"] 
  
    if (nrow(tdata)>0) {
    tt <- tab.title.std.errbar(pk_name=pk_name, 
                              drug_name=lib_TEST["drug_name",  "DESCRIPTIVE_NAME"], 
                              study_name=lib_STUDY["study_name",  "DESCRIPTIVE_NAME"], 
                              )
    mydoc <- addParagraph(mydoc, value=tt, bookmark=paste(tab.lst[i], "_title", sep=""),stylename = "Normal")
    mydoc <- addFlexTable(mydoc, myFlexTable(tdata), bookmark=tab.lst[i] )
    mydoc <- addParagraph(mydoc, value=tab.footnote(),  bookmark=paste(tab.lst[i], "_footnote", sep=""), stylename = "C-Footnote")
    }
}
  

################################################################################
################################################################################
# Replace Figure
################################################################################
################################################################################

#--------------------------------------------
# fig.title.std.errbar
#--------------------------------------------
fig.title.std.errbar <- function(log.scale="log", errorbar="SD", pk_name, drug_name, study_name,  patient=NULL, arma=NULL) {
  log.scale.name = ""
  if (tolower(log.scale)=="nonlog") log.scale.name = ""
  if (tolower(log.scale)=="log") log.scale.name = " Log-Scaled"

  if (errorbar=="SD") {key = "Mean(+SD)"}
  if (errorbar=="SE") {key = "Mean(+SE)"}
  paste(key, log.scale.name, " ", pk_name,
        " in Serum vs Nominal Sampling Day Following Multiple Subcutaneous or Intravenous Dose(s) of "
        , drug_name, " (", study_name, ")", sep="")
}


#--------------------------------------------
# fig.footnote
#--------------------------------------------
fig.footnote <- function(log.scale="log", LLOQ=0.078) {

 footnote = ""
 
  if (tolower(log.scale)=="nonlog") {
  footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }

  if (tolower(log.scale)=="log") {
  footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  }
  
  return(footnote)

}
 

#--------------------------------------------
# final write2doc
#--------------------------------------------

test.lst = as.character(setdiff(lib_TEST[, "ID"], c("drug_name", "drug_name_alias")))
fig.lst.nonlog1 = paste("fig_errbar_", gsub("_test_name", "", test.lst,  fix=TRUE), "_nonlog",sep="")
fig.lst.log1 = paste("fig_errbar_", gsub("_test_name", "", test.lst, fix=TRUE),  "_log", sep="")
fig.lst.nonlog2 = paste("fig_indiv_", gsub("_test_name", "", test.lst,  fix=TRUE), "_nonlog",sep="")
fig.lst.log2 = paste("fig_indiv_", gsub("_test_name", "", test.lst, fix=TRUE),  "_log", sep="")

fig.lst = rbind(cbind(test.lst, fig.lst.nonlog1, "nonlog", "errbar"),
                cbind(test.lst, fig.lst.log1, "log", "errbar") ,
                cbind(test.lst, fig.lst.nonlog2, "nonlog", "indiv"),
                cbind(test.lst, fig.lst.log2, "log", "indiv"  ))
                
colnames(fig.lst) = c("test.name", "fig.name", "scale", "type")  
fig.lst = data.frame(fig.lst)    
#fig.lst = filter(fig.lst, fig.lst$fig.name %in% sort(list_bookmarks(mydoc)))      
#fig.lst = fig.lst[order(fig.lst$test.name), ]

fig.lst = data.frame(fig.lst, stringsAsFactors =FALSE)
fig.lst$fig.name = as.character(fig.lst$fig.name)
 
for (i in 1:nrow(fig.lst)) {
    tdata = eval(parse(text=fig.lst$fig.name[i]))
 
   
    pk_name=lib_TEST[as.character(fig.lst$test.name[i]),  "DESCRIPTIVE_NAME"]  
    log.name = fig.lst$scale[i]
     
    #----------------------------------
    # for word
    #----------------------------------           
    # doc, Figure, Evinacumab, 
    tt <- fig.title.std.errbar(log.scale=log.name, errorbar="SD",pk_name=pk_name, drug_name=drug_name, study_name=study_name)
    mydoc <- addParagraph(mydoc, value=tt,  bookmark=paste(fig.lst$fig.name[i], "_title",sep=""), stylename = "Normal")
    
    if (fig.lst$fig.name %in% list_bookmarks(mydoc)) {
       mydoc <- addPlot2(mydoc, x=tdata, pointsize=font.size, bookmark=fig.lst$fig.name[i]) 
    }else{
       mydoc <- addPlot(mydoc, x=tdata, fun=print, pointsize=font.size)    # add into appendix
    }
    mydoc <- addParagraph(mydoc, value=fig.footnote(log.scale=log.name, LLOQ=0.078),  bookmark=paste(fig.lst$fig.name[i],"_footnote", sep=""),stylename = "C-Footnote")
 
    #----------------------------------
    # for ppt
    #----------------------------------            
    #ppt, Figure:  Evinacumab, log
 
    my.title =  paste(str_to_title(log.name), "-scale profile of", pk_name, "administrated by", drug_name, "in", study_name, sep=" ")    
                 
                
    font12_style <- textProperties(color='black',font.size = 12,
                font.weight = 'normal', font.family = 'Times New Roman' ) 
                              
    myppt <- myppt %>%
      addSlide( slide.layout = "Title and Content" ) %>%
      addTitle(value=my.title, level=6) %>%
      addPlot( function( ) print( tdata ) ) %>%
      addPageNumber() %>%
      addFooter(pot(fig.footnote(log.scale=log.name, LLOQ=0.078), font12_style))
  
  
 }
 
  return(list(mydoc=mydoc, myppt=myppt)) 
  
}

 