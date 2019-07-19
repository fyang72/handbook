
#-------------------------------------------------------------------
# setup directory
#-------------------------------------------------------------------
rm(list=ls())


# placehold for all tables and figures  
FIGURE_ALL = NULL
TABLE_ALL = NULL


MODEL.HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\"
base.model.runno = c("LN014")  # , "MM001"
final.model.runno = c("LN900")



# working directory
HOME = getwd() # "/data/BiCS_RDE_Development/shiny-server_development/pharmacometrics/R2810_popPK/"
HOME = "H:\\Pharmacometrics\\r2810-poppk\\"
setwd(HOME)

#setwd(HOME)

PARENT = dirname(HOME)



#-------------------------------------------------------------------
# Options in bookdown
#-------------------------------------------------------------------

# http://www.pzhao.org/en/post/bookdown-cheatsheet/    #   bookdown-cheatsheet      


library("rmarkdown")   # packageVersion("rmarkdown")
#library("kableExtra")   # packageVersion("rmarkdown")

set.seed(1234567)
options(digits = 3)

#knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(
  message=FALSE,
  warnings = FALSE,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  echo = FALSE
)

# output: pdf_document
# header-includes:
#   - \usepackage{color}
# http://www.sthda.com/english/wiki/create-and-format-word-documents-using-r-software-and-reporters-package



# set pander table-layout options
library(pander)
panderOptions('table.alignment.default', function(df)
  ifelse(sapply(df, is.numeric), 'right', 'left'))
panderOptions('table.split.table', Inf)
panderOptions('big.mark', ",")
panderOptions('keep.trailing.zeros', TRUE)


options(dplyr.print_min = 6, dplyr.print_max = 6)




#-------------------------------------------------------------------
# loading R script library
#-------------------------------------------------------------------

# loading regnR script functions
library(bookdown)
#source(paste0(PARENT, "/regnR/load_Rpackage.R"))
#install.packages("bookdown")
# install.packages('servr')  servr: A Simple HTTP Server to Serve Static Files or Dynamic Documents. Start an HTTP server in R to serve static files, or dynamic documents that can be converted to HTML files (e.g., R Markdown) under a given directory.




# report and facilitator
library("ReporteRs")
library("knitr")
library("magrittr")
library("shiny")
library("mrgsolve")
#library("rmarkdown")
#library("testthat")

# plots
#library("latticeExtra")
library("ggplot2")     # library("gplots")

# data manipulation
library("xtable")
library("data.table")
library("dplyr")    # not include plyr,
library("reshape2")   # not reshape
library(haven)       # read_sas 
library(readxl)      # read_excel
library(readr)     # read_csv
library(tidyverse)
library('lazyeval')

# string manipuation
library("stringr")

library(rowr)

# loading data
#library("sas7bdat")

# others
library("Hmisc")
#library("deSolve")
library("gdata")
library("PKPDmisc")

library("scales") 
#library("devtools")
#library("roxygen2")
library(RColorBrewer)

folder.loc <- paste0(PARENT, "/regnR/R")
file.lst <-list.files(path = folder.loc, pattern = ".r",  all.files = FALSE,full.names = TRUE, include.dirs = FALSE)     
file.lst <- file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]

for (ifile in 1:length(file.lst)) { 
  print(file.lst[ifile]);    
  source(file=file.lst[ifile])  }

source(paste0("./util.R"))


# 
# July 26
# August 23
# September 27
# October 25
# November 29
# 
# Best
# 
