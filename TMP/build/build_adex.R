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
#'   
build_adex <- function(adex, 
                       date_time_format = c("Ymd HMS")) {
  #"Ymd HMS"      "2018-04-30T11:27"
  #"db!Y HMS"     '10-May-2017 14:35:00'
  
    
  #--------------------------------------------  
  # Default check
  #-------------------------------------------- 
  adex = as.data.frame(adex) 
  colnames(adex) = toupper(colnames(adex))
  
  #---------------------------------------------
  # must have these variables, then fill "." if missing desired variables
  #---------------------------------------------   
  col.lst = c("STUDYID", "USUBJID",  "EXTRT", "EXDOSE",  "EXDOSU", "EXROUTE",  "EXSTDTC",  "EXENDTC")
  col.lst = setdiff(col.lst, colnames(adex))
  if (length(col.lst)>0) {
    print( paste0("Warning: ", paste0(col.lst, collapse=", "), " must be included in adex"))
    stopifnot(length(col.lst)>0) }
     
  # if missing desired variables
  col.lst = setdiff(adex.col.lst, colnames(adex))
  if (length(col.lst)>0) {print( paste0("Warning: ", paste0(col.lst, collapse=", "), " not included in adex"))}
  adex[, col.lst] = "."
  
  
  #USUBJID
  adex$CLID = adex$USUBJID 
  study.id = unique(adex$STUDYID)
  for (i in 1:length(study.id)) { 
    adex$CLID = gsub(study.id[i], "", adex$CLID, fix=TRUE)
  }
  adex$CLID = gsub("-", "", adex$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adex$CLID))
  if (length(t1)>1)  {print("the length of CLID in adex are not the same.")}
  if (t1==9) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
  if (t1==6) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
  if (t1==3) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
  if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adex !=3, 6 or 9")}
  
  adex$USUBJID <- paste(adex$STUDYID,  adex$SUBJECT, sep="-") 
  adex = adex %>% select(-SUBJECT, -CLID)
  
  
  #---------------------------------------------
  # "EXSTDTC",  "EXENDTC", "DUR", "EXFIRST", "TIME"
  #--------------------------------------------- 
  # https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
  # "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
  # "ymd HM"  "09-01-03 12:02"  
  # x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
  # parse_date_time(x, "Ymd HMS", truncated = 3)

  library(lubridate)
  if ((!all(class(adex$EXSTDTC) %in% c("POSIXct", "POSIXt" )))) { 
    adex$EXSTDTC = parse_date_time(adex$EXSTDTC, date_time_format, truncated = 3)  } 
  
  if ((!all(class(adex$EXENDTC) %in% c("POSIXct", "POSIXt" )))) { 
    adex$EXENDTC = parse_date_time(adex$EXENDTC, date_time_format, truncated = 3)  } 
  
  adex = adex %>% mutate( 
    DUR = as_numeric(difftime(EXENDTC, EXSTDTC, units = "days")))  
  
  adex = adex %>% group_by(USUBJID) %>% mutate( 
    EXFIRST =  min(EXSTDTC), 
    TIME = as_numeric(difftime(EXSTDTC, EXFIRST, units = "days")) 
  )  %>% ungroup()
  
 
  
  #---------------------------------------------
  # ARMA
  #--------------------------------------------- 
  # http://www.cwladis.com/math104/lecture5.php   
  #  subcultaneously (subQ), intramuscularly (IM), or intravenously (IV).
  #  q.h.     q.2.h.     q.3.h.    q.4.h.
  #  q.d.    (q.o.d.)
  #  b.i.d   twice a day
  #  t.i.d   three times a day
  #  q.i.d   four times a day  
  
  adex$ARMA = adex$ARMA
 
  
  #---------------------------------------------
  # "EXROUTE"    "SUBCUTANEOUS"  "INTRAVENOUS"   "INTRAMUSCULAR" "IVT"      
  #--------------------------------------------- 
  adex = adex %>% mutate(EXROUTE = toupper(EXROUTE), 
                         EXROUTE = ifelse(EXROUTE=="SC", "SUBCUTANEOUS",  
                                          ifelse(EXROUTE=="IV", "INTRAVENOUS",  
                                               ifelse(EXROUTE=="IM", "INTRAMUSCULAR", EXROUTE))), 
                                          
                         EXDOSE = as_numeric(EXDOSE), 
                         EXDOSU = as.character(EXDOSU)
  )
  
  #---------------------------------------------
  # EXSEQ
  #--------------------------------------------- 
  adex = adex %>% group_by(USUBJID) %>% mutate(EVID = ifelse(as_numeric(EXDOSE)>0, 1, 0), 
                                               EXSEQ = cumsum(EVID))
  
  adex %>% select(USUBJID, EXSTDTC, EXENDTC, DUR, EXFIRST, TIME, EVID, EXSEQ)
  
    
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  col.lst = c(adex.col.lst, setdiff(colnames(adex), adex.col.lst))
  adex = adex[, col.lst]
   
  return(adex)
  
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
check_adex <- function(adex.FINAL)   {
  
  adex = adex.FINAL
  
  # Check the number of subject 
  cat(paste("","\n","Number of subjects (exlude Screen Failure): ", length(unique(adex$USUBJID)), "\n", sep=""))
  
  # Check number of subjects within each GROUP and ARMA
  cat(paste("", "\n", "Accouting of USUBJID by ARMA...\n", sep=""))
  print(ddply(adex, "ARMA", summarise, N=fun.uniqN(USUBJID)) %>% arrange(ARMA))
  
  cat(paste("", "\n", "Accouting of USUBJID by GROUP...\n", sep=""))      
  print(ddply(adex, "GROUP", summarise, N=fun.uniqN(USUBJID)) %>% arrange(GROUP))
  
  
  #-----------------------------------------------------------------------------
  # EXSEQ, maximum number of dose,  planned number of dose vs actual number of dose
  #-----------------------------------------------------------------------------
  
  adex = adex.FINAL
  adex$WEIGHTBL = get.infor(adsl, adex, "USUBJID","WEIGHTBL")
  
  adex = adex[order(adex$ARMA, adex$EXROUTE, adex$USUBJID), ]
  col.lst = c("USUBJID", "ARMA", "TRT", "VISIT","TIMEPT","TIME","EXTRT","EXSEQ","EXROUTE","EXDOSE","EXDOSU", "EXTDOSE","EXDURAT","WEIGHTBL")
  # adex[, col.lst]
  
  # cat(paste("","\n", "R2810-ONC-1423-840-007-012, planned dose: R2810: 3 mg/kg + XRT: 9 Gy x 3 + CPA: 200 mg/m2,  actual dose: 1 mg/kg\n", sep="")) 
  
  plot(adex$EXSEQ, xlab="Subject Index", ylab="Sequence of Dose")
  
  tdata = ddply(adex, "USUBJID", summarise, MAX=fun.max(EXSEQ)) %>% arrange(MAX)
  # adex %>% group_by(USUBJID) %>% dplyr::summarise(MAX=fun.max(EXSEQ)
  
  barplot(tdata$MAX, xlab="Subject Index",ylab="Maximum # of dose",main="Maximum number of doses in each subject"); box()
  
  #-----------------------------------------------------------------------------
  # Planned dose vs actual dose
  #-----------------------------------------------------------------------------
  if (!is.null(adex$EXPDOSE)) {       
    adex$EXPDOSE = as_numeric(adex$EXPDOSE)
    adex$EXDOSE = as_numeric(adex$EXDOSE) 
    
    
    ggplot(data=adex, aes(x=EXPDOSE, y=EXDOSE, group=ARMA, col=ARMA, shape=ARMA)) +     # size
      geom_point() +   
      scale_color_manual("Dose Group", 
                         #breaks=c("A","B","C", "D", "E", "F"),  
                         values =fun.color.scheme()[1:4],  
                         labels=unique(adex$ARMA))  + 
      scale_shape_manual("Dose Group",
                         #breaks=c("A","B","C", "D", "E", "F"), 
                         values =rep(19, time=4), 
                         labels=unique(adex$ARMA))  +   
      xlab("Planned Dose") +
      ylab("Mean Concentration (mg/L)") +
      theme_bw() 
    
    adex$EXTDOSE = as_numeric(adex$EXTDOSE) 
    water.fall.plot(adex, colname="EXTDOSE", xlab.txt="Dose Index", ylab.txt="Duration of infusion (minutes)")   
  }
  
  #-----------------------------------------------------------------------------
  # Any missing dosing information
  #-----------------------------------------------------------------------------
  adex = adex.FINAL
  adex$EXTDOSE = as_numeric(adex$EXTDOSE)        
  adex = filter(adex, is.na(EXTDOSE))
  adex = adex[order(adex$ARMA, adex$EXROUTE, adex$USUBJID), ]
  
  col.lst = c("USUBJID", "ARMA",  "TRT", "VISIT","TIMEPT", "PKDAY_C","EXTRT","EXSEQ","EXROUTE","EXDOSE","EXDOSU", "EXTDOSE")
  anyna(adex[, col.lst])
  adex[, col.lst]
  
  
  #-----------------------------------------------------------------------------
  # double check the infusion duration, to determine what is NOM_DAY for "DAY 1 0HR"
  #----------------------------------------------------------------------------- 
  adex = adex.FINAL      
  adex$EXDURAT[which(adex$EXDURAT==".")]<-NA
  adex$EXDURAT = as.numeric(as.character(adex$EXDURAT))
  ids = which(!is.na(adex$EXDURAT))
  tdata = cbind(adex[ids, c("STUDYID","USUBJID", "ARMA","EXTRT")], "DAY"=adex[ids,"EXDURAT"], "HOUR"=adex[ids,"EXDURAT"]*24, "MIN"=adex[ids,"EXDURAT"]*24*60)  
  tdata = tdata[order(tdata$MIN),]
  
  water.fall.plot(tdata, colname="MIN", xlab.txt="Dose Index", ylab.txt="Duration of infusion (minutes)")   
  
  
  
  #-----------------------------------------------------------------------------
  # NTIM vs TIME
  #-----------------------------------------------------------------------------      
  adex$NTIM = as_numeric(adex$NTIM)
  adex$TIME = as_numeric(adex$TIME)
  ggplot(data=adex, aes(x=NTIM, y=TIME, group=ARMA, col=ARMA, shape=ARMA)) +     # size
    geom_point() +   
    scale_color_manual("Dose Group", 
                       #breaks=c("A","B","C", "D", "E", "F"),  
                       values =fun.color.scheme(),  
                       labels=unique(adex$ARMA))  + 
    scale_shape_manual("Dose Group",
                       #breaks=c("A","B","C", "D", "E", "F"), 
                       values =rep(19, time=25), 
                       labels=unique(adex$ARMA))  +   
    xlab("Nominal Time (day)") +
    ylab("Actual Time (day)") +
    theme_bw() 
  
  
}

#
# if (1==2) {
# 
# 
#was the dosing align with the planned dosing interval?   QW, Q2W
#
#drug administrations are missed or there are missing dates or times or there are dose changes
#
#If there is a dose reduction
#
#What should happen to the PK/PD data after missed dose or a dose without date or 
#time should also be specified. If it is decided that PK data is dropped afterwards, then they should be listed.
#
#
#
# #
# #
# #    check.adex(adex.FINAL)  
# #          
# #    ESACTINT   Actual Dose Intensity (mg/kg/wk)
# #       
# #    ESCUMDOS   Cumulative Dose Administered (mg) 
# #   ESDURN   Duration of Total Treatment  
# #  ESRELINT   Relative Dose Intensity
# #  ESTOTNUM   Total Number of Infusions
# #
# 
# 
# }   