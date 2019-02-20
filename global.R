


library(knitr)
library(rmarkdown)
library(shiny)

# library(devtools)
#devtools::install_github("jrowen/rhandsontable")

# load data
#------------------- 
library(readxl)
library(readr)        
library(haven)

 
# data manipulation
#------------------- 
library(dplyr)    # not include plyr,
library(lubridate)
library(tidyr)
library(tidyverse)
library(lazyeval)

#library(dmutate)
#library(xtable)
#library("data.table")
#library(reshape2)   # not reshape
#library(datasets)

#------------------------------------------------------------
# plots
#------------------------------------------------------------
#library(Cairo)   # to solve X11 server problem
library("ggplot2")     # library("gplots")
#library(gridExtra)
#library(ggpmisc)


#------------------------------------------------------------
# Pk specific package
#------------------------------------------------------------
library("mrgsolve") 
library("PKPDmisc")         #
library("xpose4")
#library(vpc)
 


#------------------------------------------------------------
# stats
#------------------------------------------------------------
#library(minqa)
#library(methods) 

#------------------------------------------------------------
# Reporting
#------------------------------------------------------------
#library(ReporteRs)
#library("rmarkdown")
#library(knitr) #knitting


#------------------------------------------------------------
# Shiny, Rmarkdown
#------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(DT) 
#library("plotly")



#------------------------------------------------------------
# utilities
#------------------------------------------------------------
library(scales) 
#library(lubridate)  FOR DATE AND TIME
library("stringr")
library('RColorBrewer')
library("Hmisc")    # label(adex)
library("gdata")     # trim
library("RColorBrewer")
#library(MASS)   # MUST BE FIST BEFORE dplyr
library(Rcpp)
library(xtable) #pretty tables
#library(kfigr) #figure referencing for markdown # devtools::install_github("github mkoohafkan/kfigr")
#if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
#if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 
#library(stargazer) #pretty tables










######################################################################
# Load packages
######################################################################
#rm(list = ls()) # 
# installed.packages(.Library, priority = "high"))
# installed.packages(lib.loc="./packrat/lib-R/x86_64-pc-linux-gnu/3.4.2/", priority = "high") 
                   
list.of.packages = sort((.packages()))
#list.of.packages

# [1] "grid"           "XML"            "reshape"        "pander"         "rmarkdown"      "xpose4"         "scales"         "bindrcpp"      
# [9] "latticeExtra"   "Rcpp"           "PKPDmisc"       "mrgsolve"       "gdata"          "Hmisc"          "Formula"        "survival"      
# [17] "lattice"        "forcats"        "stringr"        "purrr"          "tidyr"          "tibble"         "tidyverse"      "xtable"        
# [25] "haven"          "knitr"          "plotly"         "readr"          "reshape2"       "DT"             "ReporteRs"      "ReporteRsjars" 
# [33] "shinydashboard" "rhandsontable"  "dplyr"          "magrittr"       "readxl"         "shiny"          "RColorBrewer"   "ggplot2"       
# [41] "lazyeval"       "stats"          "graphics"       "grDevices"      "utils"          "datasets"       "methods"        "base"


#if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
#if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 

 
# 
# #------------------------------------------------------------
# # loading data
# #------------------------------------------------------------
# library(readxl)
# library(readr)        
# # library(haven)
#  
# #------------------------------------------------------------
# # data manipulation
# #------------------------------------------------------------
#  
# 
# library(magrittr)
# 
# library(haven)
# library(dplyr)    # not include plyr,
# library(tidyr)
# library(dmutate)
# library(xtable)
# #library("data.table")
# library(reshape2)   # not reshape
# library(tidyverse)
# library(lazyeval)
# library(lubridate)
# 
# #------------------------------------------------------------
# # plots
# #------------------------------------------------------------
# #library(Cairo)   # to solve X11 server problem
# #library("latticeExtra")
# library("ggplot2")     # library("gplots")
# library(gridExtra)
# library(ggpmisc)
# 
# 
# #------------------------------------------------------------
# # Pk specific package
# #------------------------------------------------------------
# library("mrgsolve") 
# library("PKPDmisc")         #
# library("xpose4")
# #library(vpc)
# #library("deSolve")
# 
# 
# #------------------------------------------------------------
# # stats
# #------------------------------------------------------------
# library(minqa)
# library(methods) 
#  
# #------------------------------------------------------------
# # utilities
# #------------------------------------------------------------
# library(scales) 
# #library(lubridate)  FOR DATE AND TIME
# library("stringr")
# library('RColorBrewer')
# library("Hmisc")
# library("gdata")
# library("RColorBrewer")
# #library(MASS)   # MUST BE FIST BEFORE dplyr
# library(Rcpp)
# library(xtable) #pretty tables
# #library(kfigr) #figure referencing for markdown # devtools::install_github("github mkoohafkan/kfigr")
# if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
# #if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 
# #library(stargazer) #pretty tables
# 
# #------------------------------------------------------------
# # Reporting
# #------------------------------------------------------------
# library(ReporteRs)
# #library("rmarkdown")
# library(knitr) #knitting
#  
# 
# #------------------------------------------------------------
# # Shiny, Rmarkdown
# #------------------------------------------------------------
# library(shiny)
# library(shinydashboard)
# library(rhandsontable)
# library(DT) 
#library("plotly")


#------------------------------------------------------------
# facilitator
#------------------------------------------------------------
#library("testthat")

 
#if("package:Rcpp" %in% search()) detach("package:Rcpp", unload=TRUE) 
#unloadNamespace("Rcpp")


  
actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"

 
 
login = NULL
login$status = TRUE
login$user.name.lst = c("training",   "feng.yang"   )

 

observeEvent(session, { 
  globalVars$login$user.name = "feng.yang"   # determineCurrentUser(session=session)
  print("current users:")
  print(globalVars$login$user.name)
  
  globalVars$login$status = globalVars$login$user.name %in% globalVars$login$user.name.lst  
})

 


# password.lst = c("12345" )
# loginTab = data.frame(user.name=user.name.lst, password=password.lst)
# rownames(loginTab) = user.name.lst


library(readxl)
library(magrittr)
library(dplyr)
file.name = "./lib/pkmeta.xlsx"
magicTab <- read_excel(file.name, sheet="cheatsheet")
magicTab <- magicTab %>% mutate(Domain=as.character(Domain), 
                                Alias=as.character(Alias), 
                                Freq=as.numeric(Freq), 
                                Label=as.character(Label), 
                                Where=as.character(Where), 
                                Who=as.character(Who))
magicTab = magicTab %>% filter(!is.na(Domain))



# scriptTab
scriptTab = read_csv("./lib/scriptTab.csv")

scriptTab = scriptTab %>% 
  mutate(DATE = as.Date(as.POSIXct(DATE, format= "%Y-%m-%d"), format = "yyyy-mm-dd"))


globalVars <- reactiveValues(login=login, 
                             magicTab=magicTab, 
                             scriptTab=scriptTab)




######################################################################
# Load local R functions
######################################################################

#folder.loc <- paste0(dirname(dirname(getwd())), "/regnR/R")
#folder.loc <- "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/regnR/R2"
ihandbook = 0

 folder.loc <- "~/regnR/R2"
 
 file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
 
 file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]
 
 for (ifile in 1:length(file.lst)) { 
   print(file.lst[ifile]);  
   source(file=file.lst[ifile])  
 }     #sys.source('file.R', envir=environment())
  

 folder.loc <- "./module/"
 file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
 file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]
 
 file.lst = file.lst[which(!substr(gsub(folder.loc, "", file.lst, fix=TRUE), 2, 2) %in% c("_"))]
  
  for (ifile in 1:length(file.lst)) { 
    print(file.lst[ifile]);  
    source(file=file.lst[ifile])  
 }     #sys.source('file.R', envir=environment())
 
 folder.loc <- "./script/"
 file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
 file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]
 
 file.lst = file.lst[which(!substr(gsub(folder.loc, "", file.lst, fix=TRUE), 2, 2) %in% c("_"))]
 
 for (ifile in 1:length(file.lst)) { 
   print(file.lst[ifile]);  
   source(file=file.lst[ifile])  
 }     #sys.source('file.R', envir=environment())
 
######################################################################
# initialization of key variables
######################################################################

# working directory
# HOME = getwd() # "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/R2810_popPK/"
# PARENT = dirname(HOME)
# setwd(HOME)


# placehold for all tables and figures  

graphics.off() 

FIGURE_ALL = NULL
TABLE_ALL = NULL

mg = 1
mkg = 2

SC = 1
IV = 2

######################################################################
# default options in shiny
######################################################################

# http://www.pzhao.org/en/post/bookdown-cheatsheet/    #   bookdown-cheatsheet      

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
options(shiny.maxRequestSize=1000*1024^2)   #  1000MBi..e 1GB
options(bitmapType='cairo')   # solve Warning: Error in grDevices::png: X11 is not available

# Load server detail objects into the global workspace.
#globalConfigurationDirectory <- "../../global_configuration/"
#source(file=paste(globalConfigurationDirectory,"load_server_configuration_objects.R",sep=""),local=TRUE)



######################################################################
# default options in bookdown
######################################################################
library("rmarkdown")   # packageVersion("rmarkdown")
#library("kableExtra")   # packageVersion("rmarkdown")

set.seed(1234567)
options(digits = 3)
 
knitr::opts_chunk$set(fig.width=9, 
                      fig.height=6, 
                      fig.path='figures/', 
                      comment = "#>",
                      collapse = TRUE,
                      warning=FALSE, 
                      message=FALSE, 
                      fig.retina=NULL, 
                      cache=FALSE, 
                      autodep=TRUE, 
                      echo=FALSE)


# echo=FALSE indicates that the code will not be shown in the final document (though any results/output would still be displayed).
# results="hide" to hide the results/output (but here the code would still be displayed).
# include=FALSE to have the chunk evaluated, but neither the code nor its output displayed.


# knitr::opts_chunk$set(echo = FALSE, cache=FALSE, message=FALSE, warning=FALSE, fig.align='center')  # results='asis', 
# + results="asis" --- this is useful when you generate a table with xtable() or stargazer(). 
# 
# + fig.cap="blah blah blah" --- this is where you'd type in the caption to a figure (presumably you'd write something more informative than blah blah blah, but you do you)
# 
# + fig.height=5 --- this is how you can customize the height of a figure (here I've set it to 5 inches)
# 
# + fig.width=5 --- this is how you can customize the width of a figure (here I've set it to 5 inches)
# 


# output: pdf_document
# header-includes:
#   - \usepackage{color}
# http://www.sthda.com/english/wiki/create-and-format-word-documents-using-r-software-and-reporters-package



######################################################################
# convention used in REGN
######################################################################
topN = 20
date_time_format = c("Ymd HMS", "mdY HMS", "bdY HMS")

testcat.lst = c("PK1", "TARGET1", "SAF1", "EFF1", "BIOMKR1", "ADA1", "AE1")

adsl.var.lst = c("STUDYID", "USUBJID",  
                 "WGTBL", 
                 "HGTBL", 
                 "AGE", "AGEU",  
                 "SEX", "SEXN", 
                 "RACE",  "RACEN", 
                 "ETHNIC","ETHNICN", 
                 "BMIBL", "BSABL", "SITEID", 
                 "SAFFL", "ENRLFL","RANDFL","PKFL","COMPLFL" )

sex.lst = c( "MALE", "FEMALE", "UNKNOWN")

ethnic.lst = c("NOT HISPANIC OR LATINO",  "HISPANIC OR LATINO")

race.lst =c("WHITE", 
              "BLACK OR AFRICAN AMERICAN",
              "ASIAN",
              "AMERICAN INDIAN OR ALASKA NATIVE",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",    
              "OTHER", 
              "UNKNOWN",
              "NOT REPORTED"
              )

 

adex.var.lst = c("STUDYID",  "USUBJID",  "ARMA", "ARMAN", "VISIT",    "VISITNUM",     "TIME",   
                 "EXTRT", "EXSEQ",  "EXDOSE",   "EXDOSU",  "EXTDOSE",  "EXROUTE", "EXROUTN", "EXDUR" ,  
                 "EXSTDTC",  "EXENDTC", "TRTSDTM")
 



dosu.lst = c("mg", "mg/kg")
admin.route.lst = c("SUBCUTANEOUS", "INTRAVENOUS", "INTRAMUSCULAR", "IVT")
 

adpc.var.lst <- c(
  "STUDYID",   "USUBJID", "ARMA",  "ARMAN",
  "VISIT",   "VISITNUM",   "PCTPT", "TIME", "NTIM", "TAD", 
  "TEST", "TESTCD", "TESTCAT", "TESTCATN",  "DVOR", "DVORU", "BLQ", "LLOQ",  "METHOD", "SAMDTTM"      
)  

 

nmdat.var.lst =  c(
  "C", "ROWID",   "ID", "ARMAN",  "TIME", "NTIM","TAD", 
  "EXTRTN", "AMT",  "RATE", "EXROUTN", "EVID",  "EXDUR",   "EXDOSE",   "EXDOSU",  "EXTDOSE",  "EXROUTE", "EXSTDTC", "EXENDTC", "TRTSDTM", 
  "TEST", "TESTCD", "TESTCAT", "TESTCATN", "DV", "DVOR", "BLQ", "LLOQ", "CMT",  "MDV", "METHOD", "SAMDTTM",  "FASFL",
  "STUDYID", "USUBJID", "ARMA",  "VISIT", "VISITNUM", "PCTPT", "DVORU", 
  
  "AGE",  "AGEU",  "SEX",  "SEXN", "RACE",  "RACEN", "ETHNIC", "ETHNICN", "WGTBL",  "HGTBL", "BMIBL",
  "CFLAG"             
)
adpx.var.lst = nmdat.var.lst
######################################################################
# default options in reportR
######################################################################






######################################################################
# default options in dplyr
######################################################################
options(dplyr.print_min = 6, dplyr.print_max = 6)





######################################################################
# default options in pander
######################################################################
# set pander table-layout options
# library(pander)
# panderOptions('table.alignment.default', function(df)
#   ifelse(sapply(df, is.numeric), 'right', 'left'))
# panderOptions('table.split.table', Inf)
# panderOptions('big.mark', ",")
# panderOptions('keep.trailing.zeros', TRUE)
 



