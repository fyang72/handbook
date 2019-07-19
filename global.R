 

library(stringr)
HOME = paste0(normalizePath("."), "/")
if (str_sub(HOME, 1, 22) == "/home/feng.yang/FYANG/") {HOME=paste0("~/YANG/")}
 
 
######################################################################
# Load packages
######################################################################
#rm(list = ls()) # 
# installed.packages(.Library, priority = "high"))
# installed.packages(lib.loc="./packrat/lib-R/x86_64-pc-linux-gnu/3.4.2/", priority = "high") 

# list.files(.libPaths()[1])
# .libPaths()
# installed.packages()
# tools:::.get_standard_package_names()
# rsconnect::appDependencies()
list.of.packages = sort((.packages())) 

# [1] "grid"           "XML"            "reshape"        "pander"         "rmarkdown"      "xpose4"         "scales"         "bindrcpp"      
# [9] "latticeExtra"   "Rcpp"           "PKPDmisc"       "mrgsolve"       "gdata"          "Hmisc"          "Formula"        "survival"      
# [17] "lattice"        "forcats"        "stringr"        "purrr"          "tidyr"          "tibble"         "tidyverse"      "xtable"        
# [25] "haven"          "knitr"          "plotly"         "readr"          "reshape2"       "DT"             "ReporteRs"      "ReporteRsjars" 
# [33] "shinydashboard" "rhandsontable"  "dplyr"          "magrittr"       "readxl"         "shiny"          "RColorBrewer"   "ggplot2"       
# [41] "lazyeval"       "stats"          "graphics"       "grDevices"      "utils"          "datasets"       "methods"        "base"
#if ("metrumrg" %in% list.of.packages) {base::detach(package:metrumrg) } 
#if ("MASS" %in% list.of.packages) {base::detach(package:MASS) } 
 
# Reporting
#-------------- 
library(knitr)
library(rmarkdown)
library(ReporteRs) 
library(officer)

 
# Shiny, Rmarkdown
#-------------------- 
library(shiny)
library(shinyAce)
library(shinydashboard)
library(rhandsontable)
library(DT) 
library(formatR)
#library("plotly")



# load data
#------------------- 
library(readxl)
library(readr)        
library(haven)  # xlsx   xlsxjars
library(xlsx)
library(xlsxjars)


# data manipulation
#------------------- 
library(dplyr)    # not include plyr, 
library(tidyr)
library(tidyverse)
library(lazyeval) 

library(dmutate)
#library(xtable)
#library(data.table)
library(reshape2)   # not reshape
#library(datasets)
library(pryr)   # Partial function application allows you to modify a function by pre-filling some of the arguments. 
                # It is particularly useful in conjunction with functionals and other function operators.

#------------------------------------------------------------
# plots
#------------------------------------------------------------
#library(Cairo)   # to solve X11 server problem
require(grid)
library("ggplot2")     # library("gplots")
library(gridExtra)
library(ggpmisc)         # add_formula_pvalue
library(VennDiagram)
# library(party)  # tree structure of the model development

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
library(chron)     # as_time
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
 
list_files_in_a_folder <- function(folder.loc="./util/", file.extension=c(".r", ".R")) {
  
  file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
  file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-(nchar(file.extension)-1), nchar(file.lst)) %in% file.extension)]
  file.lst = file.lst[which(!substr(basename(file.lst), 1, 1) %in% "_")]
  
  file.lst = file.lst[setdiff(1:length(file.lst), grep("_not_used", file.lst, fixed=TRUE))]
  
  # find a string multiple files
  # for (ifile in 1:length(file.lst)) { 
  #   print(file.lst[ifile])
  #   txt = paste0(readLines(file.lst[ifile]), collapse=" ")
  #   if ( length(grep("regnR", txt, fixed=TRUE) )) {
  #     print(file.lst[ifile])
  #     exit;
  #   }
  #    
  # }     #sys.source('file.R', envir=environment())
  
  return(file.lst)
}
  
ihandbook = 0
 
file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/util/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) { source(file=file.lst[ifile]) }     

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/module/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) {source(file=file.lst[ifile])  }     
 
file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/script/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) {source(file=file.lst[ifile])  }     

 
######################################################################
# default options in shiny
######################################################################

# http://www.pzhao.org/en/post/bookdown-cheatsheet/    #   bookdown-cheatsheet      

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
options(shiny.maxRequestSize=1000*1024^2)   #  1000MBi..e 1GB
options(bitmapType='cairo')   # solve Warning: Error in grDevices::png: X11 is not available

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
                      fig.align='center',
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
 





######################################################################
# initialization of key variables
######################################################################

server_IP_address =  NULL  #"10.244.64.97"    # NULL  #
actionButton_style ="float:left;color: #fff; background-color: #328332; border-color: #328332"

login = NULL
login$status = TRUE
login$user.name.lst = c("training",   "feng.yang"   )
login$user.name = "feng.yang"   # determineCurrentUser(session=session)
login$status = login$user.name %in% login$user.name.lst  
globalVars <- reactiveValues(login=login)


######################################################################
# convention 
######################################################################
  
library(dplyr)
library(readxl)
file_name <- paste0(HOME, "/lib/pkmeta.xlsx" )

# key data format
std_adsl <- read_excel(file_name,sheet="adsl",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adex <- read_excel(file_name,sheet="adex",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_adpc <- read_excel(file_name,sheet="adpc",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_nmdat <- read_excel(file_name,sheet="nmdat",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(standard.name))

std_convention <- read_excel(file_name,sheet="convention",col_names = TRUE)  %>% 
  as.data.frame() %>% filter(!is.na(domain))

# key variables
adsl_var_lst <- std_adsl %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adex_var_lst <- std_adex %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
adpc_var_lst <- std_adpc %>% filter(tier %in% c(1,2)) %>% pull(standard.name)
nmdat_var_lst <- std_nmdat %>% filter(tier %in% c(1,2)) %>% pull(standard.name)

# adsl_data_type
adsl_data_type <- fuzzy_match(std_adsl %>% pull(type), 
                              c("character", "numeric", "integer"))
names(adsl_data_type) = std_adsl %>% pull(standard.name)  #c("STUDYID", "SEXN", "WGTBL")

# adex_data_type
adex_data_type <- fuzzy_match(std_adex %>% pull(type), 
                              c("character", "numeric", "integer"))
names(adex_data_type) = std_adex %>% pull(standard.name)  #c("STUDYID", "SEXN", "WGTBL")

# adpc_data_type
adpc_data_type <- fuzzy_match(std_adpc %>% pull(type), 
                              c("character", "numeric", "integer"))
names(adpc_data_type) = std_adpc %>% pull(standard.name)  #c("STUDYID", "SEXN", "WGTBL")

# nmdat
nmdat_data_type <- fuzzy_match(std_nmdat %>% pull(type), 
                               c("character", "numeric", "integer"))
names(nmdat_data_type) = std_nmdat %>% pull(standard.name)  #c("STUDYID", "SEXN", "WGTBL")


# key convention
dvoru_var_lst <- std_convention %>% filter(domain=="DATA", name=="DVORU") %>% pull(value)
sex_var_lst <- std_convention %>% filter(domain=="DATA", name=="SEX") %>% pull(value)
race_var_lst <- std_convention %>% filter(domain=="DATA", name=="RACE") %>% pull(value)
ethnic_var_lst <- std_convention %>% filter(domain=="DATA", name=="ETHNIC") %>% pull(value)
dosu_var_lst <- std_convention %>% filter(domain=="DATA", name=="DOSU") %>% pull(value)
route_var_lst <- std_convention %>% filter(domain=="DATA", name=="ROUTE") %>% pull(value)
testcat_var_lst <- std_convention %>% filter(domain=="DATA", name=="TESTCAT") %>% pull(value)
timefmt_var_lst <- std_convention %>% filter(domain=="DATA", name=="TIMEFMT") %>% pull(value)

topN = 20
fuzzy_match_method = "jw"
fuzzy_match_threshold = 0.5

n_subject_showing_in_simulation = 10
nmdat.mandatory.var.lst = c("ROWID","ID","TIME","DV","CMT","MDV","AMT","RATE","EVID")

infusion_hrs_lst = c(0.5, 1, 2)
followup_period = 112   # days
simulation_delta = 1  # days




######################################################################
# default ALL for debugt purpose 
######################################################################

library(shiny)
library(ggplot2)

DATA = NULL
FIGURE = NULL
TABLE = NULL
cppModel = NULL
ctlModel = NULL
script = NULL

tdata = data.frame(xvar=1:10, yvar=1:10)
figLn <- ggplot(tdata, aes(x=xvar, y=yvar)) + geom_point() + geom_line()
figLog <- figLn + scale_y_log10()

fig <- figLn
attr(fig, 'title') <- paste0(
  "Mean(±SE) ", " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", ")")

attr(fig, 'width') <- 9
attr(fig, 'height') <- 6                                
FIGURE[["pk_mean_profile_ln"]] = fig 
FIGURE[["pk_mean_profile_ln"]]$data =  tdata


fig <- figLog
attr(fig, 'title') <- paste0(
  "Semi-Log Mean(±SE) ", " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", ")")

attr(fig, 'width') <- 9
attr(fig, 'height') <- 6                                
FIGURE[["pk_mean_profile_log"]] = fig 
FIGURE[["pk_mean_profile_log"]]$data =  tdata


tabl = data.frame(x=1, y=1)
attr(tabl, 'title') <-  "tst1"
TABLE[["TSET1"]] = tabl

tabl = data.frame(x=10, y=10)
attr(tabl, 'title') <-  "tst2"
TABLE[["TSET2"]] = tabl

tabl = data.frame(x=1, y=1)
attr(tabl, 'title') <-  "tst1"
DATA[["TSET1"]] = tabl

tabl = data.frame(x=10, y=10)
attr(tabl, 'title') <-  "tst2"
DATA[["TSET2"]] = tabl

cppModel1=mread(model='cppModel',
                project=paste0(HOME, '/cpp/'),
                quiet=TRUE,
                file=basename("LN001.cpp"))

cppModel2=mread(model='cppModel',
                project=paste0(HOME, '/cpp/'),
                quiet=TRUE,
                file=basename("LN001.cpp"))


cppModel = list(
  "LN0011.cpp"=cppModel1, 
  "LN0022.cpp"=cppModel2
) 

ctlModel = list(
  "LN001.ctl"=readLines(paste0(HOME, "/ctl/", "LN001.ctl")), 
  "LN002.ctl"=readLines(paste0(HOME, "/ctl/", "LN002.ctl"))
) 


script = list(
  "build_adsl.R"=readLines(paste0(HOME, "/script/", "build_adsl.R")), 
  "build_adex.R"=readLines(paste0(HOME, "/script/", "build_adex.R"))
)

default_DATA   = DATA
default_FIGURE = FIGURE
default_TABLE = TABLE 
default_cppModel = cppModel
default_ctlModel = ctlModel
default_script = script




