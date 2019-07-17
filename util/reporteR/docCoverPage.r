

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
docCoverPage <- function(mydoc, myppt, PKreport)   {

#lib_AUTHOR  <- convert.info(PKreport, type="AUTHOR")
#lib_TEST  <- convert.info(PKreport, type="TEST")
#lib_STUDY <- convert.info(PKreport, type="STUDY")
#

lib_AUTHOR <- PKreport$lib_AUTHOR 
lib_TEST  <- PKreport$lib_TEST 
lib_STUDY <- PKreport$lib_STUDY 

# parse lib_AUTHOR
 
author =  lib_AUTHOR["author", "NAME"]
reviewer =  lib_AUTHOR["reviewer", "NAME"]

drug_name =   lib_TEST["drug_name", "DESCRIPTIVE_NAME"]
study_name =  lib_STUDY["study_name", "DESCRIPTIVE_NAME"]
patient_characteristic = lib_STUDY["patient_characteristic",  "DESCRIPTIVE_NAME"]
 
report_title = lib_STUDY["report_title", "DESCRIPTIVE_NAME"]
report_name = lib_STUDY["report_name", "DESCRIPTIVE_NAME"]

protocol_name = lib_STUDY["protocol_name", "DESCRIPTIVE_NAME"]
protocol_title = lib_STUDY["protocol_title", "DESCRIPTIVE_NAME"]

#--------------------------------------------
# Update Cover Page        color: '#1163A5'
#--------------------------------------------
 # replace bookmarks for REPORT and PROTOCOL
bold10_style <- textProperties(color='black',font.size = 10,
                font.weight = 'bold', font.family = 'Times New Roman' )            
mydoc <- addParagraph2(mydoc, value=pot(drug_name, bold10_style), bookmark="drug_name" )
mydoc <- addParagraph(mydoc, value=pot(report_name, bold10_style),  bookmark="report_name", par.properties = parProperties(text.align="right"))


font11_style <- textProperties(color='black',font.size = 11,
                font.weight = 'normal', font.family = 'Times New Roman' ) 
mydoc <- addParagraph2(mydoc, value=pot(report_title, font11_style), bookmark="report_title")
mydoc <- addParagraph2(mydoc, value=pot(protocol_name, font11_style ), bookmark="protocol_name" )
mydoc <- addParagraph2(mydoc, value=pot(protocol_title, font11_style), bookmark="protocol_title" )
 
 
# STUDY ADMINISTRATIVE STRUCTURE #
#color : font color; e.g : color="#000000" or color = "black".
#font.size : a integer indicating the font size.
#font.weight : the font weight. Possible values are "normal" or "bold".
#font.style : the font style. Possible values are "normal" or "italic".
#underlined : a logical value specifying if the text should be underlined.
#font.family : the font family; e.g : "Arial".
#vertical.align : a character indicating font vertical alignments. Expected values are "baseline"" or "subscript" or "superscript". Default value is baseline.
#shading.color : background color of the text (e.g "#000000" or "black")
#
font12_style <- textProperties(color='black',font.size = 11,
                font.weight = 'normal', font.family = 'Times New Roman' )

# replace bookmarks 'AUTHOR' and 'REVIEWER'  
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "NAME"],font12_style), bookmark="author_coverpage" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "TITLE"],font12_style), bookmark="author_title_coverpage" )
 

mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "NAME"],font12_style), bookmark="reviewer_coverpage" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "TITLE"],font12_style), bookmark="reviewer_title_coverpage" )
 


mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "NAME"],font12_style), bookmark="scientific_writer_coverpage" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "TITLE"],font12_style), bookmark="scientific_writer_title_coverpage" )
 
               
               
               
font10_style <- textProperties(color='black',font.size = 10,
                font.weight = 'normal', font.family = 'Times New Roman' )
                
# replace bookmarks 'AUTHOR' and 'REVIEWER'  
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "NAME"],font10_style), bookmark="author" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "TITLE"],font10_style), bookmark="author_title" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "PHONE"],font10_style), bookmark="author_phone" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["author", "EMAIL"],font10_style), bookmark="author_email" )

mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "NAME"],font10_style), bookmark="reviewer" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "TITLE"],font10_style), bookmark="reviewer_title" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "PHONE"],font10_style), bookmark="reviewer_phone" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["reviewer", "EMAIL"],font10_style), bookmark="reviewer_email" )


mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "NAME"],font10_style), bookmark="scientific_writer" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "TITLE"],font10_style), bookmark="scientific_writer_title" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "PHONE"],font10_style), bookmark="scientific_writer_phone" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["scientific_writer", "EMAIL"],font10_style), bookmark="scientific_writer_email" )


mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["statistical_programmer", "NAME"],font10_style), bookmark="statistical_programmer" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["statistical_programmer", "TITLE"],font10_style), bookmark="statistical_programmer_title" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["statistical_programmer", "PHONE"],font10_style), bookmark="statistical_programmer_phone" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["statistical_programmer", "EMAIL"],font10_style), bookmark="statistical_programmer_email" )

mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["quality_control", "NAME"],font10_style), bookmark="quality_control" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["quality_control", "TITLE"],font10_style), bookmark="quality_control_title" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["quality_control", "PHONE"],font10_style), bookmark="quality_control_phone" )
mydoc <- addParagraph2(mydoc, value= pot(lib_AUTHOR["quality_control", "EMAIL"],font10_style), bookmark="quality_control_email" )





 


# Add a Title slide  
myppt <- myppt %>% 
  addSlide( slide.layout = "1_Title Slide" ) %>% 
  addTitle(paste("Key Result Data for ", study_name, sep="") ) %>% #set the main title
  addSubtitle(paste(paste(author, reviewer, sep=" & "), "\n", format(Sys.time(), "%b %d %Y"),sep=""))       #set the sub-title
 


 return(list(mydoc=mydoc, myppt=myppt))
 
 }
 
  