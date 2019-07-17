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

build_adpx <- function(adex, adpc)      {
 
   
#  adsl = adsl_final
#  adex = adex_final
#  adpc = adpc_final

  #demog.lst = c( "AGE",  "WEIGHTBL", "HEIGHTBL", "BMIBL" ,   "BSABL",    "SEX" ,     "SEXN",     "RACE",     "RACEN" ,  "ETHNIC",   "ETHNICN")
  #adex = adex[, setdiff(colnames(adex), demog.lst)]
  #adpc= adpc[, setdiff(colnames(adpc), demog.lst)]
 
   
  
  adex$AMT = as_numeric(adex$EXTDOSE)          # TOTAL DOSE in mg in each adminstration, but not the whole duration of treatment
  adex$EXDURAT = as_numeric(adex$EXDURAT)          # TOTAL DOSE in mg in each adminstration, but not the whole duration of treatment
  
  #RATE   
  adex$RATE <- 0 
  ids = which(adex$EXROUTE == "IV INFUSION")
  adex$RATE[ids] <- adex$AMT[ids]/adex$EXDURAT[ids]   # 2 hour infusion
  
  #CMT
  adex$CMT="."
  ids = which(as.character(adex$EXROUTE) %in% c("IV BOLUS", "IV INFUSION"));  adex$CMT[ids]= 2
  ids = which(as.character(adex$EXROUTE) %in% c("IVT", "SC", "IM")); adex$CMT[ids] = 1
  
  # make adpc nonmem-ready or compatible dataset 
  adpc = adpc %>% mutate(AMT = ".", 
                         DVOR = DVOR,   
                         DV = ifelse(is.na(log(as_numeric(DVOR))), ".", u.signif(log(as_numeric(DVOR)), digits=3)),
                         MDV = ifelse(is.na(DV), 1, 0), 
                         EVID = 0, 
                         CMT = 2,      # Default
                         RATE = ".")
                         
  # make adex nonmem-ready or compatible dataset                    
  adex = adex %>% mutate(AMT = EXTDOSE, 
                         DVOR = ".",
                         DV = ".",
                         MDV = 1,                          
                         EVID = 1, 
                         CMT = CMT, 
                         RATE = RATE)
                         
  # merge adex and adpc 
  col.lst = unique(c(colnames(adex), colnames(adpc)))
  adex[, setdiff(col.lst, colnames(adex))] = "."
  adpc[, setdiff(col.lst, colnames(adpc))] = "."
  #adpx = bind_rows(adex, adpc) 
  adpx = rbind(adex, adpc)
    
    
#  # merge with adsl
#  col.lst = c("USUBJID", "AGE",  "WEIGHTBL", "HEIGHTBL", "BMIBL" ,   "BSABL",    "SEX" ,     "SEXN",     "RACE",     "RACEN" ,  "ETHNIC",   "ETHNICN")
#  adpx = left_join(adpx, adsl[, col.lst], by="USUBJID")
  
  # add nonmem ID;  
  adpx = adpx %>% mutate(ID = as.integer(as.factor(USUBJID)))
 
  adpx[which(is.na(adpx$DV)), "DV"] = "."
 
 
  # ANY NA??
  nacols <- function(df) { 
     colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }
  anyna <- function(df) {
     df = data.frame(df, check.names = FALSE)      
     t.df = data.frame(t(df), check.names = FALSE)
     return(df[nacols(t.df), nacols(df)])  }
     
  if (length(nacols(adpx))>0)  {print(nacols(adpx))}
  stopifnot(length(nacols(adpx))==0)
    
       

       
  if (!"ORGID" %in% colnames(adpx))  {adpx$ORGID = "."}   #if it is for adpx simulation (using R or nonmem)  
  # final output
  #key.col.lst = c("STUDYID","USUBJID","ID","ARMAN","VISITNM","NTIM","TIME","DVOR","DV","MDV","CMT","AMT","RATE","EVID","EXSEQ","EXROUTN","NOTE")
  key.col.lst = c("STUDYID","USUBJID","ORGID", "ID", "ARMA","ARMAN","VISITNM","NTIM","TIME","TIMEPT","TEST","TESTN","DVOR","STDUNIT","DV","MDV","CMT","AMT","RATE","EVID",
  "EXSEQ","EXROUTE", "EXROUTN")  
  #"AGE",  "WEIGHTBL", "HEIGHTBL", "BMIBL" ,   "BSABL",    "SEX" ,     "SEXN",     "RACE",     "RACEN" ,  "ETHNIC",   "ETHNICN") 

  sec.col.lst = setdiff(colnames(adpx),key.col.lst)    # secondary
  col.lst = setdiff(c(key.col.lst, sec.col.lst), colnames(adpx))
  if (length(col.lst)>0) {print(paste("mising colnames of ", paste(col.lst, collapse=", ", sep=""), " in adpx", sep=""))}
  adpx[, col.lst] = "."
  #adpx = data.frame(adpx[, c(key.col.lst, sec.col.lst)])
  adpx =  adpx[, c(key.col.lst )] 

  adpx = adpx[order(adpx$STUDYID, adpx$ARMA, adpx$USUBJID, adpx$TIME, -adpx$EVID), ]
  rownames(adpx) <- 1:nrow(adpx) 
    
     
  
  return(adpx)
  }
  
  
  


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
  check_adpx <- function(adpx.FINAL)   {
  
  #-------------------------------------------------
  # clean adpx for use in nonmem dataset
  #-------------------------------------------------

  adpx.CLN = adpx.FINAL     # create an cleaned adpx
 
  adpx.col.lst = c(  "USUBJID",   "ARMA",     "TIMEPT",    "NTIM",      "TIME",      
      "EXSEQ",     "EXROUTE",    "EXDOSE",    "EXDOSU",    "AMT",       "RATE",      
      "EVID",      "DVOR",      "STDUNIT",        "MDV",       "CMT") 
       
  adpx.key.col = c(  "USUBJID",   "ARMA",     "CYCLE", "TIMEPT",    "NTIM",      "TIME",      
      "EXSEQ",     "EXROUTE",    "EXDOSE",    "EXDOSU",    "AMT",       "RATE",      
      "EVID",      "DVOR",      "STDUNIT",        "MDV",       "CMT")  
     

  if (1==2) {
  # record -wise 
  adpx = adpx %>% filter(ARMA != "Placebo")
  adpx = adpx %>% filter(TIME >=0)
    
  # column-wise
  minimal.col.lst = c("USUBJID", "ARMA","ARMAN","ID", "TIME", "TEST","DVOR", "DV","MDV","CMT","AMT","RATE", "EVID", "EXSEQ","EXROUTN","AGE","WEIGHTBL","SEXN","RACEN","ETHNICN")
  adpx = adpx[, minimal.col.lst]
  }
  
  #-----------------------------------------------------   
  # remove explicit placebo subject  
  #----------------------------------------------------- 
  adpx = adpx.CLN       
  subj.lst = unique(adpx$USUBJID[which(adpx$ARMA %in% c("PLACEBO"))])
  if (length(subj.lst)>0) {print(subj.lst)  }
  
  tdata <- adpx %>% filter(toupper(ARMA)!="PLACEBO")     # adpx[which(!adpx$USUBJID %in% subj.lst),]
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to explicit placebo subject",sep=""))
  adpx = tdata
  
  adpx.CLN = adpx
   

  #-----------------------------------------------------
  # remove implicit placebo subject, we may suspect such subjects may be dosed with placebo
  #-----------------------------------------------------
  adpx = adpx.CLN

  adpx$DVOR = regnR::as_numeric(adpx$DVOR) 
  tdata = adpx %>% filter(EVID==0) %>% group_by(USUBJID) %>% summarise(SUSPECT_SUBJ = all(DVOR %in% c(0, NA)))   # 
  which(tdata$SUSPECT_SUBJ)

  subj.lst = tdata %>% filter(!SUSPECT_SUBJ) %>% select(USUBJID) %>% .[[1]]
  tdata = adpx %>% filter(USUBJID %in% subj.lst )
  
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to implicit placebo subject",sep=""))
  adpx = tdata  
  
  adpx.CLN = adpx
   
 
  #-----------------------------------------------------
  # check TIME....
  #-----------------------------------------------------
  adpx = adpx.CLN
  adpx$TIME = regnR::as_numeric(adpx$TIME)  
    
  # checking TIME in adpx, must not be "NA" or ".", 
  tdata = adpx[which(!is.na(adpx$TIME)),  ]
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to is.na(TIME)",sep=""))
  adpx = tdata
  
  # checking TIME in adpx, must be >=0
  tdata = adpx[which(adpx$TIME>=0),  ]
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to TIME<0",sep=""))
  adpx = tdata
  
  # records must start the first dose, i.e. EVID=1, TIME=0 and EXSEQ=1
  tdata = adpx[which(adpx$TIME==0 & adpx$EVID==1 & adpx$EXSEQ==1),  ]      # R2810-ONC-1423-840-003-011    R2810-ONC-1423-840-008-001  why EXSEQ=0
  #print(tdata)
  
  adpx[which(adpx$USUBJID %in% setdiff(adpx$USUBJID, tdata$USUBJID)), ]   
  
  adpx.CLN = adpx
   
   
  #-----------------------------------------------------
  # check unrealistic peak : trough ratios in DVOR
  #-----------------------------------------------------
  if (1==2 ) {
  adpx = adpx.CLN
  adpx$TIME = regnR::as_numeric(adpx$TIME)
  adpx$DVOR = regnR::as_numeric(adpx$DVOR)  

  # only concern about PRE and 0HR
  ids =  regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0      
  adpx = adpx[ids, ]
        
  adpx$DVOR = regnR::as_numeric(adpx$DVOR)      
  adpx$EVENT = NA            
  adpx$EVENT[which(regexpr('PRE', adpx[, "TIMEPT"]) >=0)] = "PRE"
  adpx$EVENT[which(regexpr('0HR', adpx[, "TIMEPT"]) >=0)] = "POST"
  
  # extract the DVOR for POST and PRE, and calculate the RATIO
  tt = acast(adpx, USUBJID+ARMA+EXSEQ ~ EVENT, fun.aggregate = fun.mean, value.var="DVOR")
  tt = data.frame(colsplit(rownames(tt),"_",c("USUBJID","ARMA","EXSEQ")), tt)
  tt = tt[order(tt$USUBJID, tt$EXSEQ),]
  rownames(tt) = 1:nrow(tt)
  tt$PRE = regnR::as_numeric(tt$PRE)
  tt$POST = regnR::as_numeric(tt$POST)  
  tt$PRE[which(tt$PRE==0)] = 0.078
  tt$POST[which(tt$POST==0)] = 0.078
  tt$RATIO = regnR::as_numeric(tt$POST)/regnR::as_numeric(tt$PRE) 
  tt$RATIO[which(tt$RATIO>10)] = 10
  tt = tt[order(tt$RATIO),]
  
  # what is the mean value for POST and PRE
  acast(adpx, ARMA ~ EVENT, fun.aggregate = fun.mean, value.var="DVOR")
  barplot(tt$RATIO[which(!is.na(tt$RATIO))])
   
  # identify the abnormal POST and PRE for each dosing period, USUBJID + EXSEQ
  
  tt2 = tt[which(tt$RATIO<1.5), ]
  tt2 = tt2[order(tt2$USUBJID, tt2$EXSEQ),]  
  tt2
   
  # Finally, exclude those observations (DVOR) at POST and PRE for each abnormal dosing event  
  adpx = adpx.CLN
  ids = adpx$EVID==0 & 
        (regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0) &          
        paste(adpx$USUBJID,adpx$EXSEQ,sep="-") %in% paste(tt2$USUBJID,tt2$EXSEQ,sep="-")  
  adpx[ids, adpx.col.lst]
  
  tdata = adpx[!ids, ]
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to abnormal peak:trough ratio",sep=""))
  adpx = tdata
 
  adpx.CLN = adpx
  } 


  #-----------------------------------------------------
  # no dosing event, but having concentration, should removed. 
  #-----------------------------------------------------
  adpx = adpx.CLN
    
  tdata = adpx  
  tdata$AMT = regnR::as_numeric(tdata$AMT)
  tdata$DVOR = regnR::as_numeric(tdata$DVOR)  
  subj.adex = unique(tdata[which(tdata$EVID==1 & tdata$AMT!=0),"USUBJID"] )     # subjects having valid dosing
  subj.adpc = unique(tdata[which(tdata$EVID==0 & !is.na(tdata$DVOR)),"USUBJID"] )   # subject having valid conc
   
  # identify subjects who have adpc information but no dosing information                                               
  subj.lst = setdiff(subj.adpc, subj.adex)
  tdata = filter(adpx, !(USUBJID %in% subj.lst))  
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to having adpc information but no dosing information ",sep=""))
  adpx = tdata
  
  adpx.CLN = adpx
  
  
  
  #-----------------------------------------------------
  # whether or not remove all DV = 0 or BLQ data entries;
  #----------------------------------------------------- 
  adpx = adpx.CLN
    
  ids <- (regnR::as_numeric(adpx$DVOR) == 0 | is.na(regnR::as_numeric(adpx$DVOR)))  &  
          regnR::as_numeric(adpx$EVID) == 0 
  #adpx[ids, "DVOR"] = ".";     adpx[ids, "MDV"] = 1
  
  tdata = adpx[!ids, ]      #  adpx[ids, adpx.col.lst]
  if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to BLQ",sep=""))
  adpx = tdata
  
  adpx.CLN = adpx


  return(adpx.CLN)
  }
  
  
  
  
 
     
################################################################################
################################################################################
# generate NONMEM compatiable "adpx"
################################################################################
################################################################################
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # cautions!   NO " " empty space in nonmem dataset
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #nmdat$TRTGROUP = gsub(" ", "", nmdat$TRTGROUP)
  #nmdat$TIMEPT = gsub(" ", "-", nmdat$TIMEPT)
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
build_nmdat <- function(adpx, path="./data/nmdat.csv", descp="") {

  adpx = as.data.frame(adpx)
 
  adpx$AMT[which(is.na(adpx$AMT))] = "."
  adpx$DVOR[which(is.na(adpx$DVOR))] = "."
    
  adpx[is.na(adpx)] = "."
  

  # remove NA entries
  if (length(nacols(adpx))>0) {
    print(nacols(adpx))
    print(anyna(adpx))
  }
  adpx[is.na(adpx)] = "."
  
  adpx$EVID = as_numeric(adpx$EVID) 
  adex = adpx[which(adpx$EVID!=0), ]; t.adex = paste(adex$USUBJID, adex$TIME, sep="")
  adpc = adpx[which(adpx$EVID==0), ]; t.adpc = paste(adpc$USUBJID, adpc$TIME, sep="")
  adpc = adpc[which(!t.adpc %in% t.adex), ]
  adpx = rbind(adex, adpc)
  adpx = adpx %>% arrange(ARMA, USUBJID, TIME, -EVID) 
  #adpx = adpx[order(adpx$STUDYID, adpx$USUBJID, adpx$TIME, -adpx$EVID), ]  # no ARMA, Since it becomes complicated for multilple doses per subject


  nmdat = adpx
  colnames(nmdat) <- gsub(".", "_", colnames(nmdat), fixed=TRUE)
  colnames(nmdat) <- gsub(" ", "_", colnames(nmdat), fixed=TRUE)
 
  # remove "," and " " in some columns 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(" ", "-", nmdat[, icol])} 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(",", "-", nmdat[, icol])} 
 
  
  key.col.lst = c("USUBJID","ID","ARMA","VISIT","TIMEPT","TIME","TEST","DVOR","DV","MDV","BLQ","LLOQ","CMT","AMT","RATE","EVID","EXSEQ",       
  "WEIGHTBL", "HEIGHTBL", "BMIBL",  "BSABL", "AGE",  "SEX" ,  "SEXN",     "RACE",     "RACEN" ,  "ETHNIC",   "ETHNICN" ) 
  sec.col.lst = setdiff(colnames(nmdat),key.col.lst)    # secondary
  
  col.lst = setdiff(c(key.col.lst, sec.col.lst), colnames(nmdat))
  if (length(col.lst)>0) {print(paste("mising colnames of ", paste(col.lst, collapse=", ", sep=""), " in adpx", sep=""))}
  nmdat[, col.lst] = "."
  nmdat = nmdat[, c(key.col.lst, sec.col.lst)]


  # drop some columns
  drop.lst =c("DRUG", "DRUG=DROP",
              "STUDYID",  "STUDYID=DROP",
              "USUBJID",  "USUBJID=DROP",
              "ORGID",    "ORGID=DROP",
              "PHASE",    "PHASE=DROP",
              "EXTRT",    "EXTRT=DROP",
              "GROUP",    "GROUP=DROP",
              "ARMA",     "ARMA=DROP",
              "COHORT",   "COHORT=DROP",
              "TRT",      "TRT=DROP",
              "CYCLE",    "CYCLE=DROP",
              "VISIT",    "VISIT=DROP",
              "TIMEPT",   "TIMEPT=DROP",
              "TEST",     "TEST=DROP",
              "STDUNIT",  "STDUNIT=DROP",
              #"BLQ",      "BLQ=DROP",
              "EXROUTE",  "EXROUTE=DROP",
              "EXDOSU",   "EXDOSU=DROP",
              "EXTDOSU",  "EXTDOSU=DROP",
              "EXDURAT",  "EXDURAT=DROP",
              "EXDINT",   "EXDINT=DROP",
              "SEX",      "SEX=DROP",
              "AGEU",     "AGEU=DROP",
              "RACE",     "RACE=DROP",
              "ETHNIC",   "ETHNIC=DROP", 
              "COMMENT",  "COMMENT=DROP",
              "NOTE",     "NOTE=DROP")
  drop.lst = matrix(drop.lst, ncol=2, byrow = TRUE )
  colnames(drop.lst) <- c("STD.NAME", "NM.NAME")
  
  u.str.subs <- function(x, str1="", str2="") { 
    stopifnot(length(str1)==length(str2))
    
    for (i in 1:length(str1)) { 
    ids <- which(x==str1[i])
    x[ids] <- str2[i]
    }
    return(x)
    } 
      
  for (i in 1:nrow(drop.lst)) {
     colnames(nmdat) = u.str.subs(colnames(nmdat), str1=drop.lst[i,"STD.NAME"], str2=drop.lst[i,"NM.NAME"])   }


  # add header
  for (i in colnames(nmdat)) {
     nmdat[,i] <- as.character(nmdat[,i]) 
     nmdat[,i] <- gsub(" ", "_", nmdat[,i], fixed=TRUE)
     }
     
  nmdat = cbind(ROWID=1:nrow(nmdat), nmdat)
  nmdat <- rbind("C2"=colnames(nmdat),  nmdat)
  #nmdat["C1", 1] <- descp  # in order to use PDxPop
  rownames(nmdat)[2:nrow(nmdat)] <- 1:(nrow(nmdat)-1)

  # paste(colnames(nmdat), collapse=" ")
  #nonmem.file.name <- paste(".\\NONMEM\\R1193_HV_1219_simulation_01", ".csv", sep="")
  write.table(nmdat, path, sep=",", col.names = FALSE, row.names = TRUE, quote = FALSE)

 
  paste(c("$INPUT  C=DROP ", colnames(nmdat)), sep=" ", collapse=" ")
  }
  









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
   cross_check <-function(adsl, adex, adpc, adlb) {
       
      #----------------------------------------------------------------------------- 
      # check adpc$ARMA against  adsl$ARMA
      #-----------------------------------------------------------------------------
      print("Checking adpc$ARMA against adsl$ARMA ...") 
      
      adpc =  adpc.FINAL
      adsl =  adsl.FINAL
      
      adpc = adpc[which(!duplicated(adpc$USUBJID)), ]
      rownames(adpc) = adpc$USUBJID
      subj.lst = as.character(adpc$USUBJID)
      
      col.lst = c("USUBJID", "ARMA", "GROUP")
      OUT = cbind(adpc[,col.lst], adsl[match(subj.lst, rownames(adsl)), col.lst])
      
      tt = data.frame(adpc[,col.lst] == adsl[match(subj.lst, rownames(adsl)), col.lst])
      tt = OUT[c(which(tt$ARMA=="FALSE"), which(tt$GROUP=="FALSE")), ]
      if (nrow(tt)>0)  print(tt)
        
        
   }
   
   
        
        
        