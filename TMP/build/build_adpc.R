

##############################################################################

# Purpose:
#-----------
# Create standardlized adpc dataset by merging dataset (LIMS and clical server)

# Input and output
#------------------
# Input: adex, pk

# Output
# 1) csv dataset, its location specified by users
# 2)
##############################################################################

# TEST                    REGN1193
# EXTRT:                  REGN1193  adex$TEST  
# ARM /ARMCD /ARMN:       Group-A 0.05 mg/kg IV
# COHORT:                 Group-A
# EXDOSCH:                0.05 mg/kg
# TRT/TRTN:               0.05 mg/kg,  similar as EXDOSCH
# TRTGROUP:               1A, 2B
# DOSEPLAN_LIM:           0.05                  from pk dataset\



build_adpc <- function(adpc, 
                       date_time_format = c("Ymd HMS", 
                                            "db!Y HMS"     # '10-May-2017 14:35:00'
                                            )) {
   
  #--------------------------------------------  
  # Default setup
  #--------------------------------------------  
  #adpc = as.data.frame(adpc) 
  colnames(adpc) = toupper(colnames(adpc))
  
  
  #---------------------------------------------
  # must have these variables, then if the desired variables missing, fill with "." 
  #---------------------------------------------   
  col.lst =  c(
    "STUDYID",   "USUBJID", "ARMA",  "VISIT", "TIMEPT", 
    "TEST", "METHOD",  "DVOR", "STDUNIT", "SAMDTTM", "BLQ", "LLOQ"             
  )  
  col.lst = setdiff(col.lst, colnames(adpc))
  if (length(col.lst)>0) {
    print( paste0("Warning: ", paste0(col.lst, collapse=", "), " must be included in adpc"))
    stopifnot(length(col.lst)==0) }
  
  # if missing desired variables
  col.lst = setdiff(adpc.col.lst, colnames(adpc))
  if (length(col.lst)>0) {print( paste0("Warning: ", paste0(col.lst, collapse=", "), " not included in the original adpc"))}
  adpc[, col.lst] = "."
  
  

  #----------------------------------------------------------------------------- 
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"   
  #-----------------------------------------------------------------------------
  adpc$STUDYID = adpc$STUDYID 
  
  #USUBJID
  adpc$CLID = adpc$USUBJID 
  study.id = unique(adpc$STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  for (i in 1:length(study.id)) { 
    adpc$CLID = gsub(study.id[i], "", adpc$CLID, fix=TRUE)
  }
  adpc$CLID = gsub("-", "", adpc$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adpc$CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)>1)  {print("the length of CLID in adpc are not the same.")}
  if (t1==9) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
  if (t1==6) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
  if (t1==3) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
  if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adpc !=3, 6 or 9")}
  
  adpc$USUBJID <- paste(adpc$STUDYID,  adpc$SUBJECT, sep="-") 
  adpc = adpc %>% select(-SUBJECT, -CLID)
 
  
  #----------------------------------------------------------------------------- 
  # ARMA LEVEL:   "ARMA" "ARMAN"  
  #----------------------------------------------------------------------------- 
  adpc$ARMA = adpc$ARMA
  
  
  #----------------------------------------------------------------------------- 
  # TIME LEVEL: "VISIT"    "VISITNUM"  "TIMEPT"   "NTIM/NOM_DAY"  "TIME/PKDAY_C"
  #-----------------------------------------------------------------------------
  adpc$TIMEPT = toupper(adpc$TIMEPT) 
  
  adpc$VISIT = toupper(adpc$VISIT)  # paste("Visit ", adpc$VISITNM, sep="") # u.add.prefix(adpc$VISITNM, prefix="", add.number.zero=3), sep="") 
  adpc$VISITNUM = extractExpr(adpc$VISIT, "([0-9]*\\.?[0-9]+)")  %>% as_numeric()
  adpc$VISIT = paste0("VISIT ", adpc$VISITNUM)
  
  if (all(adpc$NTIM=="."))  {
    adpc = adpc %>% select(-NTIM) %>% 
          left_join(parseTIMEPT(adpc %>% pull(TIMEPT) %>% unique()) %>% select(TIMEPT, NTIM), by="TIMEPT")
  }
        
  
  # TIME
  # -----------------------------------------------  
  #UTC is not a time zone, but a time standard that is the basis for civil time and time zones worldwide. This means 
  # that no country or territory officially uses UTC as a local time.
  
  # SAMDTTM:  start Date/time of sampling   TRTSTDTM    TRTSDTM
  #adex$SAMDTTM = as.POSIXct(adex$EXSTDT*60*60*24 + adex$EXSTTM, origin="1960-01-01", tz="GMT")
  #adex$SAMDTTM = as.POSIXct(adex$TRTSDTM, origin="1960-01-01", tz="GMT")
  #adex$SAMDTTM = as.POSIXlt(paste(DATE, "T", TIME1, sep=""), format = "%Y-%m-%dT%H:%M", tz="GMT")
  #strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "%a, %d %b %Y %H:%M:%S %z")  

  # https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
  # "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
  # "ymd HM"  "09-01-03 12:02"  
  # x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
  # parse_date_time(x, "Ymd HMS", truncated = 3)
  
  
 
  #----------------------                  
  # METHOD (SOP), TEST
  #---------------------- 
  library(lubridate) 
  adpc <- adpc %>%  mutate(METHOD = METHOD, 
                           TEST = TEST,
                           DVOR = DVOR,    # keep those ADA results (which is character-based)
                           STDUNIT = tolower(STDUNIT), 
                           
                           DVOR = ifelse(STDUNIT=="ng/ml", as_numeric(DVOR)/1000, DVOR), 
                           STDUNIT = ifelse(STDUNIT=="ng/ml", "mg/L", STDUNIT), 
                           
                           STDUNIT = ifelse(STDUNIT=="ug/ml", "mg/L", STDUNIT), 
                           
                           
                           LLOQ = ifelse(is.na(as_numeric(LLOQ)), ".", as_numeric(LLOQ)),
                           BLQ = ifelse(as_numeric(DVOR)<LLOQ, 1, 0), 
                           BLQ = ifelse(is.na(BLQ), ".", BLQ) 
                           #SAMDTTM = ifelse((!all(class(SAMDTTM) %in% c("POSIXct", "POSIXt" ))), 
                            #                parse_date_time(SAMDTTM,  date_time_format, truncated = 3),  SAMDTTM)
  )   
  
  if ((!all(class(adpc$SAMDTTM) %in% c("POSIXct", "POSIXt" )))) { 
    adpc$SAMDTTM = parse_date_time(adpc$SAMDTTM, date_time_format, truncated = 3)
  } 
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  col.lst = c(adpc.col.lst, setdiff(colnames(adpc), adpc.col.lst))
  adpc = adpc[, col.lst]
  
  
  return(adpc)
  
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
check_adpc <- function(adpc.FINAL) {
  
  adpc =  adpc.FINAL   
  
  #Ensure that the following columns are numeric and not text: TIME, COBS, WT, AGE, AMT and DOSEs
  
  
  # if DVOR is a character column, therefore want to find out what character values exist
  
  # check what character values are present
  #unique_non_numerics(adpc$DVOR)
  # BQLFLAG which takes a value of "0" if there is a numerical value in CObs and "1" if there is "BQL" in CObs.
  
  # if don't manually specify to handle NA COBS, will also get NA values for BQLFLAG
  adpc <- adpc %>% mutate(BLQ_FLAG = ifelse(is.na(DVOR), 0, 
                                            ifelse(DVOR == "BLQ", 1, 0)),
                          DVOR = as_numeric(DVOR))
  
  #----------------------------------------------------------------------------- 
  # check PKDAY_C(time) against NOM_DAY(NTIM)
  #----------------------------------------------------------------------------- 
  print("Checking PKDAY_C(time) against NOM_DAY(NTIM) ...") 
  #Consistency checks should be in place to check that relative times of blood sample reflect the planned relative time, and especially that they are on the expected side of drug administration, i.e. if they are planned to be after drug administration then they actually are after drug administration! If these can be captured early on, then the data manager can ask the site to pay particular attention to these issues.
  #
  
  
  adpc =  adpc.FINAL
  ids = order(abs(adpc$PKDAY_C - adpc$NOM_DAY), decreasing=TRUE)[1:10]
  adpc[ids, c("USUBJID", "ARMA", "PKDAY_C", "NOM_DAY")]
  
  adpc$NTIM = as.my.numeric(adpc$NTIM)
  adpc$TIME = as.my.numeric(adpc$TIME)
  ggplot(data=adpc, aes(x=NTIM, y=TIME, group=ARMA, col=ARMA, shape=ARMA)) +     # size
    geom_point() +   
    scale_x_continuous(breaks=seq(0,max(adpc$TIME), by=7),minor_breaks=seq(0,max(adpc$TIME),by=1))  +        
    scale_color_manual("Dose Group", 
                       #breaks=c("A","B","C", "D", "E", "F"),  
                       values =fun.color.scheme() ,  
                       labels=unique(adpc$ARMA))  + 
    scale_shape_manual("Dose Group",
                       #breaks=c("A","B","C", "D", "E", "F"), 
                       values =rep(19, time=100), 
                       labels=unique(adpc$ARMA))  +   
    xlab("Nominal Time (day)") +
    ylab("Actual Time (day)") +
    theme_bw() 
  
  #-----------------------------------------------------------------------------
  # Double check DAY 1 PRE, its NOM_DAY AND NOM_HR, then update NOM_DAY for "DAY 1 PRE"
  #-----------------------------------------------------------------------------
  print("Checking DAY-1, DAY 1 PRE ...") 
  
  adpc =  adpc.FINAL
  ids = which(adpc$TIME<0)
  
  tt = cbind(
    adpc[ids, c("USUBJID", "NOM_DAY", "TIMEPT")], 
    signif(adpc$TIME[ids], digits=5), 
    signif(adpc$TIME[ids]*24, digits=5), 
    signif(adpc$TIME[ids]*24*60,digits=5)) 
  colnames(tt) = c("USUBJID", "NOM_DAY", "TIMEPT", "DAY", "HOUR", "MIN")
  
  #tt = tt[which(!duplicated(tt)), ]
  tt = tt[order(tt$TIMEPT),]
  print(tt)
  
  unique(tt$TIMEPT)
  plot(tt$DAY, xlab="Subject Index", ylab="Actual Time (Day)")
  
  #-----------------------------------------------------------------------------
  # Double check who do not have predose or post-dose quantifiable sample."
  #-----------------------------------------------------------------------------
  print("Checking who do not have predose or post-dose quantifiable sample ...") 
  
  subj1 <- unique(adpc$USUBJID[which(adpc$CONCN!=0)])  
  subj2 <- unique(adpc$USUBJID[which(adpc$TIME<=0 & adpc$CONCN==0  )])
  if (length(setdiff(subj1, subj2))>0) {
    subj.lst <- setdiff(subj1, subj2)
    print.subj.info(subj.lst, message="who do not have predose sample.")  
    
    #adpc[which(adpc$USUBJID %in% subj.lst), ]
  } 
  
  if (length(setdiff(subj2, subj1))>0)  {  
    subj.lst <- setdiff(subj2, subj1)
    print.subj.info(subj.lst, message="who do not have post-dose quantifiable PK sample.")   
  }
  
  
  #-----------------------------------------------------------------------------
  # Number of quantifiable measurement per subject
  #-----------------------------------------------------------------------------
  
  adpc = adpc.FINAL
  #adpc = adpc[which(adpc$SOP=="PCL3950"), ]  # PK samples 
  adpc = adpc[which(adpc$DVOR!="0"), ]
  tt = aggregate(adpc$DVOR, by=list(adpc$USUBJID, adpc$ARMA), length)
  colnames(tt) <- c("USUBJID", "ARMA", "N")
  tt = tt[order(tt$N), ]
  
  barplot(tt$N, names.arg=tt$USUBJID)
  box()
  
  
  
  
}

