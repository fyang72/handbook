library(shiny)
library(dplyr)
 
######################################################################
# initialization of key variables
######################################################################
 
#HOME = "~/handbook/" 
HOME = normalizePath(".")

server_IP_address = NULL  #"10.244.106.127"   # NULL  #
server.IP.address = NULL  #"10.244.106.127"    # NULL  #

actionButton_style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"


login = NULL
login$status = TRUE
login$user.name.lst = c("training",   "feng.yang"   )
login$user.name = "feng.yang"   # determineCurrentUser(session=session)
login$status = login$user.name %in% login$user.name.lst  
globalVars <- reactiveValues(login=login)

# placehold for all tables and figures  

graphics.off() 

FIGURE_ALL = NULL
TABLE_ALL = NULL

mg = 1
mkg = 2

SC = 1
IV = 2


######################################################################
# convention 
######################################################################
topN = 20
date_time_format = c("Ymd HMS", "mdY HMS", "bdY HMS", "dbY HMS")

testcat.lst = c("PK1", "TARGET1", "SAF1", "EFF1", "BIOMKR1", "ADA1", "AE1")

adsl.var.lst = c("STUDYID", "USUBJID",  
               "AGE",  "AGEU",  "SEX",  "SEXN", "RACE",  "RACEN", "ETHNIC", "ETHNICN", 
               "WGTBL", "HGTBL", "BMIBL", "BSABL","SITEID", 
               "PKFL", "FASFL", "SAFFL", "ENRLFL", "RANDFL", "COMPLFL" )

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



adex.var.lst = c("STUDYID",  "USUBJID",  "ARMA", "ARMAN", "VISIT",    "VISITNUM",  "TIME",   
               "EXTRT", "EXDOSE",   "EXDOSU",  "EXTDOSE",  "EXDUR", "EXROUTE", "EXROUTN",   
               "EXSTDTC",  "EXENDTC", "TRTSDTM")



dvoru.lst = c("mg/L")
dosu.lst = c("mg", "mg/kg")
admin.route.lst = c("SUBCUTANEOUS", "INTRAVENOUS", "INTRAMUSCULAR", "IVT")


adpc.var.lst <- c(
"STUDYID",   "USUBJID", "ARMA",  "ARMAN",
"VISIT",   "VISITN",   "PCTPT", "TIME", "NTIM",  
"TEST", "TESTN", "TESTCD", "TESTCAT",   "DVOR", "DVORU", "BLQ", "LLOQ",  "METHOD", "SAMDTTM"      
)  


nmdat.mandatory.var.lst = c("ROWID","ID","TIME","DV","CMT","MDV","AMT","RATE","EVID")
nmdat.var.lst =  c(
"C", "ROWID",  "ID", "ARMAN", "VISITNUM", "TIME", "NTIM",
"TESTN", "DV", "DVOR", "BLQ", "LLOQ", "CMT",  "MDV",
"EXTRTN", "AMT",  "RATE", "EXROUTN", "EVID",  
"AGE",  "AGEU",  "SEX",  "SEXN", "RACE",  "RACEN", "ETHNIC", "ETHNICN", 
"WGTBL", "HGTBL", "BMIBL", "BSABL","SITEID", 

"STUDYID", "USUBJID", "ARMA",  "VISIT",  "PCTPT", 
"TEST", "TESTCD", "TESTCAT",  "DVORU", "METHOD", "SAMDTTM",  
"EXTRT", "EXDOSE",   "EXDOSU",  "EXTDOSE", "EXDUR", "EXROUTE", "EXSTDTC", "EXENDTC", "TRTSDTM",

"PKFL", "FASFL", "SAFFL", "ENRLFL", "RANDFL", "COMPLFL",
"CFLAG"             
)
adpx.var.lst = nmdat.var.lst




library(dplyr)
library(readxl)
std_nmdat <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="nmdat",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adsl <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="adsl",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adex <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="adex",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adpc <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="adpc",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adpx <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="adpx",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_convention <- read_excel(paste0(HOME, "/lib/pkmeta.xlsx"),sheet="convention",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(domain))


nmdat_var_lst <- std_nmdat %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adsl_var_lst <- std_adsl %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adex_var_lst <- std_adex %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adpc_var_lst <- std_adpc %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adpx_var_lst <- std_adpx %>% filter(tier %in% c(1,2)) %>% pull(standard.name)

dvoru_var_lst <- std_convention %>% filter(domain=="DVORU") %>% pull(value)
sex_var_lst <- std_convention %>% filter(domain=="SEX") %>% pull(value)
race_var_lst <- std_convention %>% filter(domain=="RACE") %>% pull(value)
ethnic_var_lst <- std_convention %>% filter(domain=="ETHNIC") %>% pull(value)
dosu_var_lst <- std_convention %>% filter(domain=="DOSU") %>% pull(value)
route_var_lst <- std_convention %>% filter(domain=="ROUTE") %>% pull(value)
testcat_var_lst <- std_convention %>% filter(domain=="TESTCAT") %>% pull(value)
timefmt_var_lst <- std_convention %>% filter(domain=="TIMEFMT") %>% pull(value)

topN = 20
n_subject_showing_in_simulation = 10
nmdat.mandatory.var.lst = c("ROWID","ID","TIME","DV","CMT","MDV","AMT","RATE","EVID")

infusion_hrs_lst = c(0.5, 1, 2)
followup_period = 112   # days
simulation_delta = 1  # days

######################################################################
# Load packages
######################################################################
#rm(list = ls()) # 
# installed.packages(.Library, priority = "high"))
# installed.packages(lib.loc="./packrat/lib-R/x86_64-pc-linux-gnu/3.4.2/", priority = "high") 

list.of.packages = sort((.packages())) 

# [1] "grid"           "XML"            "reshape"        "pander"         "rmarkdown"      "xpose4"         "scales"         "bindrcpp"      
# [9] "latticeExtra"   "Rcpp"           "PKPDmisc"       "mrgsolve"       "gdata"          "Hmisc"          "Formula"        "survival"      
# [17] "lattice"        "forcats"        "stringr"        "purrr"          "tidyr"          "tibble"         "tidyverse"      "xtable"        
# [25] "haven"          "knitr"          "plotly"         "readr"          "reshape2"       "DT"             "ReporteRs"      "ReporteRsjars" 
# [33] "shinydashboard" "rhandsontable"  "dplyr"          "magrittr"       "readxl"         "shiny"          "RColorBrewer"   "ggplot2"       
# [41] "lazyeval"       "stats"          "graphics"       "grDevices"      "utils"          "datasets"       "methods"        "base"
#if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
#if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 

#------------------------------------------------------------
# Reporting
#------------------------------------------------------------
library(knitr)
library(rmarkdown)
#library(ReporteRs) 
library(officer)


#------------------------------------------------------------
# Shiny, Rmarkdown
#------------------------------------------------------------
library(shiny)
library(shinyAce)
library(shinydashboard)
library(rhandsontable)
library(DT) 
#library("plotly")


# load data
#------------------- 
library(readxl)
library(readr)        
library(haven)


# data manipulation
#------------------- 
library(dplyr)    # not include plyr, 
library(tidyr)
library(tidyverse)
library(lazyeval)

library(dmutate)
#library(xtable)
#library(data.table)
#library(reshape2)   # not reshape
#library(datasets)

#------------------------------------------------------------
# plots
#------------------------------------------------------------
#library(Cairo)   # to solve X11 server problem
library("ggplot2")     # library("gplots")
library(gridExtra)
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
# utilities
#------------------------------------------------------------

# library(devtools) 
library(scales) 
library(lubridate)  
library("stringr")
library('RColorBrewer')
library("Hmisc")     
library("gdata")     # trim
library("RColorBrewer")
#library(MASS)   # MUST BE FIST BEFORE dplyr
library(Rcpp)
library(xtable) #pretty tables
#library(packrat)
#library("testthat")
#library(kfigr) #figure referencing for markdown # devtools::install_github("github mkoohafkan/kfigr")
#if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
#if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 
#library(stargazer) #pretty tables



######################################################################
# Load local R functions
######################################################################

#folder.loc <- paste0(dirname(dirname(getwd())), "/regnR/R")
#folder.loc <- "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/regnR/R2"
ihandbook = 0

folder.loc <- "~/handbook/util"

file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     

file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]

for (ifile in 1:length(file.lst)) { 
  print(file.lst[ifile]);  
  source(file=file.lst[ifile])  
}     #sys.source('file.R', envir=environment())


folder.loc <- "~/handbook/module/"
file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]

file.lst = file.lst[which(!substr(gsub(folder.loc, "", file.lst, fix=TRUE), 1,1) %in% c("_"))]

for (ifile in 1:length(file.lst)) { 
  print(file.lst[ifile]);  
  source(file=file.lst[ifile])  
}     #sys.source('file.R', envir=environment())

folder.loc <- "~/handbook/script/"
file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]

file.lst = file.lst[which(!substr(gsub(folder.loc, "", file.lst, fix=TRUE), 1,1) %in% c("_"))]

for (ifile in 1:length(file.lst)) { 
  print(file.lst[ifile]);  
  source(file=file.lst[ifile])  
}     #sys.source('file.R', envir=environment())

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
 



