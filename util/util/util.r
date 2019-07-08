################################################################################
################################################################################
# common functions used
################################################################################
################################################################################
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
lstAppend <- function(lstptr, label, obj) {lstptr[[deparse(substitute(label))]] <- obj; lstptr }



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
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

##############################################################################
#  functions 
##############################################################################  
#--------------------------------------------- 
#  nacols,  allna
#--------------------------------------------- 
nacols <- function(df) { 
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }
anyna <- function(df) {
  df = data.frame(df, check.names = FALSE)      
  t.df = data.frame(t(df), check.names = FALSE)
  return(df[nacols(t.df), nacols(df)])  }
###############################################################
# How to convert all INT to numeric apply to all columns
###############################################################

#Suppose you want to transform all your int columns to numeric, you can do so using one pipe:
#myDf %>%  mutate_each_( funs(as.numeric(.)), names( .[,sapply(., is.integer)] ))

# head(  pkpd %>% mutate_each(funs(replace(., .=="." , "BLOQ"))  ))
replace_NA <- function(df, value=0.078, by="BLQ") {
  df  %>% mutate_each(funs(replace(., .==0.078, by))  )  }


############################################################################## 
## a useful function: rev() for strings
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
############################################################################## 




  #-----------------------------------------------------------------------------
  # load the result for diagnostic plots
  #-----------------------------------------------------------------------------  
  param2 <- function(mod, PARAM) {
    PARAM = paste(paste(names(PARAM), "=", PARAM, sep=""), collapse=", ")
    eval(parse(text=paste("mod0 <- mod %>% param(",PARAM,")",sep=""))) }
    
    
     
# 
#Here is a more general way to achieve your goal of transforming column types:
#


#5
#down vote http://stackoverflow.com/questions/31734469/r-readr-single-col-types
#As of readr 0.2.2 we can do something like this to read a csv with all columns as character:
#
#read_csv("path/to/file",col_types = cols(.default = col_character()))
# read_csv(df, col_types = cols(.default = "d", date = "D", xxx = "i"))
#
#

      
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
    
  # find out the top 2 in each group by (group_by)
  #top2x <- function(df) { df[tail(order(df$x),2),] }
 #  assigned Q to weight per subject                      weight, no TEST, no group_by
 #  assigned Q to AUC group per subject                   TEST
 #  assign Q to concen at each time across dose groups    TEST, NTIM
 # ttp://stackoverflow.com/questions/16184947/cut-error-breaks-are-not-unique
  # approach-1 : unique           OR ntile
  # approach-2
  #breaks = c(-Inf,quantile(a[,paste(i,1,sep=".")], na.rm=T),Inf)
  #breaks = breaks + seq_along(breaks) * .Machine$double.eps
  # approach-3:
  # a <- c(1,1,1,2,3,4,5,6,7,7,7,7,99,0.5,100,54,3,100,100,100,11,11,12,11,0)
  # ar<-rank(a,ties.method = "first")
  # decile <- cut(ar, quantile(ar, probs=0:10/10), include.lowest=TRUE, labels=FALSE) 
#
#  a <- c(1,1,1,2,3,4,5,6,7,7,7,7,99,0.5,100,54,3,100,100,100,11,11,12,11,0)
#  breaks=quantile(a, quartile=c(0, 0.25, 0.5, 0.75, 1), type=2, na.rm=TRUE)
#  
#  
#  breaks = c(-Inf,breaks, Inf)
#  
#  breaks = breaks + seq_along(breaks) * .Machine$double.eps 
#  cbind(a, as.character(cut(a, breaks, include.lowest = TRUE, right = TRUE, ordered_result = TRUE)  ))
#  cbind(a,  (cut(a, breaks, include.lowest = TRUE, right = TRUE, ordered_result = TRUE)  ), 
#        as.character(cut(a, breaks, include.lowest = TRUE, right = TRUE, ordered_result = TRUE)  ))
#  
#  
    
      
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
  pasteCol <-function(adpk, col.lst, sep="~") {  

   t1 =NULL
   if (length(col.lst)>1) {
     for (i in 1:length(col.lst)) {t1 = paste(t1, adpk[, col.lst[i]], sep="~") }
   }else{
     t1 = adpk[, col.lst]
   }
   return(t1)
   } 
        
  
  
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
unique_non_numerics <-function (x, na.rm = TRUE) 
{
    if (na.rm) 
        x <- x[!is.na(x)]
    xn <- suppressWarnings(as_numeric(x))
    unique(x[is.na(xn)])
} 

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
#u.signif <- function(x, digits){
#  return(gsub("\\.$", "", formatC(signif(x,digits=digits), digits=digits, format="fg", flag="#")))
#}






 #msg <- try(source('ae-175.r')) ; rm.trymsg(msg)

 #### The function rm.trymsg() is this:
#' Extracts the time matched concntration vs effect data
#'
#'
#'
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  rm.trymsg <- function(msg) {
   if (class(msg)=='try-error') {
     cat('============',tblid,'failed =============\n')
     return(FALSE)
   }
   if (data.class(msg)=='list'& unlist(msg)[[1]]=='bad.table') {
     cat('============',tblid,'failed ===== bad.table ==========\n')
     return(FALSE)
   }
   cat('=========',tblid,'succeeded =========\n')
   TRUE
 }
     

  ##############################################################################  
  # read in libSOP and generate a dataframe
  ##############################################################################  
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
  convert.info <- function(lib, type="SOP") {
    libSOP <- as.matrix(lib[which(lib[, 1] == type),])
    colnames(libSOP) <- libSOP[1,]
    lib.SOP <- libSOP[2:nrow(libSOP), 2:ncol(libSOP), drop = FALSE]
    rownames(lib.SOP) <- lib.SOP[, "ID"]
    
     lib.SOP = data.frame(lib.SOP)

  return(lib.SOP)}










   
        
         
  
  