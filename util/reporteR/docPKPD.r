
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
docPKPD <- function(FIGURE_ALL, TABLE_ALL, mydoc, myppt, PKreport,             
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
#lib_DRUG  <- convert.info(PKreport, type="DRUG")
#lib_TEST  <- convert.info(PKreport, type="TEST")
#lib_STUDY <- convert.info(PKreport, type="STUDY")
#
lib_AUTHOR <- PKreport$lib_AUTHOR 
lib_TEST  <- PKreport$lib_TEST 
lib_STUDY <- PKreport$lib_STUDY 

drug_name =   lib_TEST["drug_name", "DESCRIPTIVE_NAME"]
study_name =  lib_STUDY["study_name", "DESCRIPTIVE_NAME"]


patient_characteristic = lib_STUDY["patient_characteristic",  "DESCRIPTIVE_NAME"]
 






#--------------------------------------------------------
# PK-PD relationship, DOUBLE-Y, PK vs. target
#--------------------------------------------------------

write2doc_tmp <- function(mydoc, myppt, FIGURE_ALL, 
         figure.key.name, figure.descriptive.name) { 

# or manually select them
plot.lst =  names(FIGURE_ALL) 
plot.lst = plot.lst[grep(figure.key.name, plot.lst)]
plot.lst.org = plot.lst

plot.lst = gsub(paste("fig_", figure.key.name,"_", sep=""), "", plot.lst, fix=TRUE)
plot.lst = gsub("_nonlog", "", plot.lst, fix=TRUE)
plot.lst = gsub("_log", "", plot.lst, fix=TRUE)
plot.lst
if (length(plot.lst)==0) { return(list(mydoc=mydoc, myppt=myppt))}
 
 
plot.lst = data.frame(plot.lst.org, tstrsplit(plot.lst, "_vs_",  fixed=TRUE, fill=NA))
colnames(plot.lst) = c("PLOT.NAME", "PD.NAME", "PK.NAME")
plot.lst

#plot.lst = data.frame(plot.lst, stringsAsFactors =FALSE)
#plot.lst$fig.name = as.character(plot.lst$fig.name)
 
for (i in 1:nrow(plot.lst)) {
  pd.name = as.character(plot.lst[i, "PD.NAME"])
  pk.name = as.character(plot.lst[i, "PK.NAME"]) 
  plot.name = as.character(plot.lst[i, "PLOT.NAME"])
   
  if (pk.name==pd.name) next
  
  tt <- print.std.title(type="figure"
                        , key = figure.descriptive.name
                        , log.scale.name = ""
                        , y.name = paste(lib_TEST[paste(pd.name,"_test_name",sep=""),  "DESCRIPTIVE_NAME"], " and ", 
                                         lib_TEST[paste(pk.name,"_test_name",sep=""),  "DESCRIPTIVE_NAME"], " ", sep="")
                        , x.name = "Nominal Sample Time (Day) "
                        , patient.name = paste(patient_characteristic, " ", sep="")  
                        , dosing.name = paste("Following Multiple Subcutaneous or Intravenous Dose(s) of ", drug_name, " ", sep="") 
                        , by.name = "Dose Regimen "
                        , study.name = study_name)
                        

  if (figure.key.name == "doubleY") {
    mydoc <- addParagraph(mydoc, value=tt, bookmark=paste(plot.name, "_title", sep=""), stylename = "Normal")  
    mydoc <- addPlot(mydoc, fun=function() print(grid.draw(FIGURE_ALL[[plot.name]])),  bookmark=plot.name, width = 6.4, height = 4.8)   
    mydoc <- addParagraph(mydoc, value=fig.footnote(),  bookmark=paste(plot.name, "_footnote", sep=""), stylename = "C-Footnote")
  
    myppt <- myppt %>%
      addSlide( slide.layout = "Title and Content" ) %>%
      addTitle(tt, level=6) %>%
      addPlot( function()   print(grid.draw(FIGURE_ALL[[plot.name]])     )) %>%
      addPageNumber()
  }
  
  if (figure.key.name != "doubleY") {
    mydoc <- addParagraph(mydoc, value=tt, bookmark=paste(plot.name, "_title", sep=""), stylename = "Normal")   
    mydoc <- addPlot2(mydoc, x= FIGURE_ALL[[plot.name]],  bookmark=plot.name)   
    mydoc <- addParagraph(mydoc, value=fig.footnote(),  bookmark=paste(plot.name, "_footnote", sep=""), stylename = "C-Footnote")
  
    myppt <- myppt %>%
      addSlide( slide.layout = "Title and Content" ) %>%
      addTitle(tt, level=6) %>%
      addPlot( function( )   print(FIGURE_ALL[[plot.name]])     ) %>%
      addPageNumber()
  }
  
  
  #ppt, Figure:  TRIG.PCHG, nonlog

 
}
 return(list(mydoc=mydoc, myppt=myppt))
 }


tt= write2doc_tmp(mydoc, myppt, FIGURE_ALL, 
         figure.key.name = "sigmoid", 
         figure.descriptive.name = "Sigmoid Plot of ")   


tt= write2doc_tmp(tt$mydoc, tt$myppt, FIGURE_ALL, 
         figure.key.name = "histerias", 
         figure.descriptive.name = "Histerias Plot of ")   


tt= write2doc_tmp(tt$mydoc, tt$myppt, FIGURE_ALL, 
         figure.key.name = "doubleY", 
         figure.descriptive.name = "Double-Y Plot of ")   

 return(tt)
 }
 
  
      