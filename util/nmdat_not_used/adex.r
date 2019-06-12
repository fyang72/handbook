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
  ADEX2adex <- function(ADEX, adsl) {
   
  ##############################################################################
  # adex
  ############################################################################## 
  # NOTE: 
  # 1) DOSE.LEVEL in string format 
  # 2) adex$USUBJID has nothing to do with adsl$USUBJID or adpp$USUBJID
  #    the final USUBJID = expand.grid(adsl$USUBJID + adex$USUBJID)
  # 3) default CMT for IV is 2, SC is 1.
  # if input REG1.ROUTE is an integer (EXROUTN)!  
  

  #
#  adsl = adsl.lst[[pop.std[i,"population"]]] 
#  adpp$USUBJID = adsl$USUBJID         ###############
#  ADEX = ADEX0[which(ADEX0$STUDYID %in% c("study1", "study2")), ]
#
#  ADEX$GROUP = u.add.prefix(ADEX[, "GROUP"],"", 3)
#  ADEX[2, "GROUP"] = "001"
#  ADEX[2, "START_DAY"] = "14"
#  ADEX 

  # ADEX must be unique in an aspect of STUDYID, GROUP, START_DAY
  ADEX = as.data.frame(ADEX)
  rownames(ADEX) = paste(ADEX$STUDYID,"_", ADEX$GROUP, "_DAY", ADEX$START_DAY, sep="")
  
  ##############################################################################      
  ##############################################################################
  #  from ADEX to adex
  ############################################################################## 
  ############################################################################## 

  # ----------------------------------------------------------------------------  
  # Expand DOSE_LEVEL into c(STUDYTID, GROUP, DOSE_ID, DOSE_LEVEL, START_DAY)
  # ----------------------------------------------------------------------------  
  adex = NULL
  
  STUDYID = NULL
  GROUP = NULL
  DOSE_ID = NULL           # GROUP_ID + DOSE_LEVEL_ID   001-001
  DOSE_LEVEL = NULL
  START_DAY = NULL
  for (i in 1:nrow(ADEX)) {
     t.level = eval(parse(text=paste("c(", ADEX[i, "DOSE_LEVEL"],")", sep="") )) 
     STUDYID = c(STUDYID, rep(as.character(ADEX[i,"STUDYID"]), time=length(t.level))) 
     GROUP   = c(GROUP, rep(as.character(ADEX[i,"GROUP"]), time=length(t.level)))      
     DOSE_ID = c(DOSE_ID, paste( u.add.prefix(ADEX[i,"GROUP"],"", 3), u.add.prefix(1:length(t.level), "", 3), sep="-"))
     DOSE_LEVEL = c(DOSE_LEVEL, t.level)
     START_DAY = c(START_DAY, rep(as.character(ADEX[i,"START_DAY"]), time=length(t.level)))
  }     
  adex = data.frame(cbind(STUDYID, GROUP, DOSE_ID, START_DAY, DOSE_LEVEL))
 
 
  # ----------------------------------------------------------------------------    
  # copy the annotation informatino for each studyid + group + start_day
  # ----------------------------------------------------------------------------    
  col.lst = setdiff(colnames(ADEX), c("STUDYID", "GROUP", "DOSE_ID","DOSE_LEVEL", "START_DAY"))
  adex[, col.lst] = ADEX[match(paste(adex$STUDYID,"_", adex$GROUP, "_DAY", adex$START_DAY, sep=""), rownames(ADEX)), col.lst ]
  rownames(adex) = paste("row-", 1:nrow(adex), sep="")
  adex = adex[order(adex$STUDYID, adex$DOSE_ID), ]
  
  
  # ----------------------------------------------------------------------------  
  # expand the actual dosing regimen Q?W
  # ----------------------------------------------------------------------------    
  tt = NULL
  adex$START_DAY = as_numeric(adex$START_DAY)
  adex$NUM_DOSE = as_numeric(adex$NUM_DOSE)
  adex$DOSE_FREQ = as_numeric(adex$DOSE_FREQ)
  for (i in 1:nrow(adex)) {
    t.end = ifelse(adex[i, "DOSE_ID"] ==adex[i+1, "DOSE_ID"], adex[i+1, "START_DAY"], adex[i, "NUM_DOSE"]*adex[i, "DOSE_FREQ"])                                            
    if (is.na(adex[i+1, "DOSE_ID"])) {t.end = adex[i, "NUM_DOSE"]*adex[i, "DOSE_FREQ"]}
    DTIME=seq(adex[i, "START_DAY"], t.end-1, by=adex[i, "DOSE_FREQ"])       ##############-1  here!!!!
    tt = rbind(tt, cbind(adex[rep(i, time=length(DTIME)), ], "DTIME"=DTIME ))
  }
  adex = tt 
  
 
  # ----------------------------------------------------------------------------  
  #  apply this dosing regimen to all subjects 
  # ----------------------------------------------------------------------------  
  adsl = as.data.frame(adsl)
  adsl$ORGID = adsl$USUBJID
  rownames(adsl) = adsl$USUBJID
  rownames(adex) = paste("ROW-", 1:nrow(adex), sep="") 
  tt = expand.grid( "USUBJID"=adsl$USUBJID, "ROW"=rownames(adex))
  tt = cbind(tt, adex[match(tt$ROW, rownames(adex)), ])
  tt = cbind(tt, adsl[match(tt$USUBJID, rownames(adsl)), setdiff(colnames(adsl),"USUBJID")])
  tt = tt[order(tt$STUDYID, tt$USUBJID, tt$DOSE_ID ), ]
  tt$USUBJID = paste(tt$STUDYID, tt$USUBJID, tt$DOSE_ID, sep="-")   # STUDYID + adsl$USUBJID + DOSE_ID
  adex = tt 
  adex0 = adex
  
  
  
  ############################################################################## 
  ############################################################################## 
  #  go through all the necessary meta information for adex
  ############################################################################## 
  ############################################################################## 

  # ----------------------------------------------------------------------------  
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"  "SUBJECT"  "SUBJECTN"
  #----------------------------------------------------------------------------- 
  
  # DRUG and USUBJID
  adex = adex0
  
  adex$STUDYID  = adex$STUDYID
  adex$USUBJID  = adex$USUBJID  # paste(adex$STUDYID, adex$DOSE_ID, sep="-")
  adex$SUBJECTN =  substr(adex$USUBJID, nchar(adex$USUBJID)-11, nchar(adex$USUBJID))    # xxxx-001-001    # which corresponds to the adsl and adpp
  adex$SUBJECTN = as_numeric(gsub("-", "", adex$SUBJECTN, fix=TRUE))
  adex$SUBJECTN[which(is.na(adex$SUBJECTN))] = 0  # if there are some error whic produce NA
  
  # ARMA LEVEL: "PHASE" "ARMA"  "ARMAN" "COHORT" "COHORTN"  "TRT" "TRTN" 
  #-----------------------------------------------------------------------------
  col.lst = c("PHASE", "GROUP", "GROUPN", "ARMA",  "ARMAN", "COHORT", "COHORTN",  "TRT", "TRTN")
  adex[, setdiff(col.lst, colnames(adex))] = "" 
  
  
  dose =  add_prefix(adex$EXDOSE, prefix="", digits=3)      
  adex$ARMA    = "" # paste(adex$EXDINT, "*", n.dose,  " ", dose, " ", adex$EXDOSU, " ", route.selection, sep="")
  adex$ARMAN   = ""
  
  adex$GROUPN = adex$GROUP  ####!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #-----------------------------------------------------------------------------   
  # TIME LEVEL: 1) "CYCLE" "VISIT"  "VISITNM" "TIMEPT" "NOM_DAY" "NTIM"  
  #             2) "PKDAY_C" "TIME" "EXFIRST"   
  #-----------------------------------------------------------------------------
  col.lst = c("CYCLE", "VISIT",  "VISITNM", "TIMEPT",  "NTIM")  
  adex[, setdiff(col.lst, colnames(adex))] = ""
  
  adex$TIME = adex$DTIME
  adex$NTIM = adex$DTIME
   
  
  #----------------------------------------------------------------------------- 
  # EXDOSE LEVEL: "EXTRT"   "EXSEQ"    "EXROUTE"  "EXROUTN"  "EXDOSE"   "EXDOSU"
  #    "EXTDOSE"  "EXTDOSU"  "EXDURAT"  "EXDINT"   "EXDINTN"  
  #-----------------------------------------------------------------------------
  col.lst = c("EXSEQ",    "EXROUTE",  "EXROUTN",  "EXDOSE",   "EXDOSU", 
              "EXTDOSE",  "EXTDOSU",  "EXDURAT",  "EXDINT",   "EXDINTN")
  adex[, setdiff(col.lst, colnames(adex))] = " "
  
  adex$EXTRT = adex$EXTRT
  
  
  #---------------------------------------------
  # EXDOSE LEVEL:   EXROUTE   EXROUTN
  #---------------------------------------------   
  adex$EXROUTE = adex$ROUTE
  
  # EXROUTE
  EXROUTE.INFO = matrix(NA, nrow=5, ncol=4)
  colnames(EXROUTE.INFO) = c( "ID", "STD.NAME", "SHORT.NAME",  "EXROUTN")
  EXROUTE.INFO[1, ] = c("SUBCUTANEOUS",	         "SUBCUTANEOUS",	        "SC",     	"1")
  EXROUTE.INFO[2, ] = c("INTRAVENOUS",	         "INTRAVENOUS",	          "IV BOLUS",	      "2")
  EXROUTE.INFO[3, ] = c("INTRAVENOUS INFUSION",	 "INTRAVENOUS INFUSION",  "IV INFUSION",	      "3")
  EXROUTE.INFO[4, ] = c("INTRAMUSCULAR",	       "INTRAMUSCULAR",	        "IM",	      "4")
  EXROUTE.INFO[5, ] = c("IVT",	                 "IVT"	,                 "IVT",	    "5")
  EXROUTE.INFO = data.frame(EXROUTE.INFO)
  rownames(EXROUTE.INFO) = EXROUTE.INFO[, "SHORT.NAME"]
  adex$EXROUTN = EXROUTE.INFO[match(adex$EXROUTE, rownames(EXROUTE.INFO)), "EXROUTN"]
  adex$EXROUTE = EXROUTE.INFO[match(adex$EXROUTE, rownames(EXROUTE.INFO)), "SHORT.NAME"]
  
  #---------------------------------------------
  # EXDOSE LEVEL:   EXDOSE   EXDOSU   EXDURAT
  #--------------------------------------------- 
  adex$EXDOSE = adex$DOSE_LEVEL
  adex$EXDOSU = adex$DOSE_UNIT
  
  adex$EXDURAT = adex$INFUSION_HOUR/24    # from hour to day
  #  ids = which(adex$EXROUTE %in% c("INTRAVENOUS INFUSION")) 
  #  adex$EXDURAT[ids] = paste(adex$EXDURAT,"HR", sep="")    # Dose Duration, in hours
  
  #---------------------------------------------
  # EXDOSE LEVEL:   EXDINT   EXDINTN
  #---------------------------------------------   
  adex$EXDINT = adex$DOSE_FREQ
  adex$EXDINTN = adex$DOSE_FREQ
  # http://www.cwladis.com/math104/lecture5.php   
  #  subcultaneously (subQ), intramuscularly (IM), or intravenously (IV).
  #  q.h.     q.2.h.     q.3.h.    q.4.h.
  #  q.d.    (q.o.d.)
  #  b.i.d   twice a day
  #  t.i.d   three times a day
  #  q.i.d   four times a day 
  ids = which(round(adex$DOSE_FREQ/7,digits=0)>0);   {adex$EXDINT[ids]  = paste("Q",round(adex$DOSE_FREQ[ids]/7,digits=0),"W",sep="") }    # Dose Interval, in weeks 
  ids = which(round(adex$DOSE_FREQ/7,digits=0)==0);  {adex$EXDINT[ids]  = paste("Q",round(adex$DOSE_FREQ[ids],digits=0),"D",sep="") }    # Dose Interval, in weeks 
  ids = which(adex$DOSE_FREQ==1/2);  adex$EXDINT[ids]  = "BID"     # Dose Interval, in weeks 
  ids = which(adex$DOSE_FREQ==1/3);  adex$EXDINT[ids]  = "TID"     # Dose Interval, in weeks 
  ids = which(adex$DOSE_FREQ==1/4);  adex$EXDINT[ids]  = "QID"     # Dose Interval, in weeks 
  ids = which(adex$DOSE_FREQ==7/2);  adex$EXDINT[ids]  = "BIW"     # Dose Interval, in weeks 
  
  #---------------------------------------------
  # EXDOSE LEVEL:   EXSEQ
  #--------------------------------------------- 
  tt <- aggregate(adex$TIME, by=list(adex$USUBJID), fun.uniqN)
  EXSEQ = NULL
  for (i in 1:nrow(tt)) { EXSEQ = c(EXSEQ, seq(1, tt[i,"x"], by=1))}
  adex$EXSEQ = EXSEQ
 
  
  #---------------------------------------------
  # EXDOSE LEVEL:   ARMA
  #--------------------------------------------- 
  #tt = adex[which(!duplicated(paste(adex$STUDYID, adex$DOSE_ID, adex$DOSE_LEVEL, sep=""))), ]
  tt = adex[which(!duplicated(adex$USUBJID)), ]
    
  dose = tt$EXDOSE # add_prefix(tt$EXDOSE, prefix="", digits=3)     #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  tt$ARMA    = paste(dose, " ", tt$DOSE_UNIT, " ",  tt$EXDINT, "*", tt$NUM_DOSE, " ", tt$ROUTE, sep="")
  tt = aggregate(tt$ARMA, by=list(tt$USUBJID),  function(x) paste(x, collapse="+"))
  colnames(tt) = c("USUBJID", "ARMA")
  rownames(tt) = tt$USUBJID
  
  adex$ARMA = tt[match(adex$USUBJID, rownames(tt)), "ARMA"]
  adex$ARMAN = as.integer(as.factor(adex$ARMA))

  #---------------------------------------------
  # EXDOSE LEVEL:   EXTDOSE  EXTDOSU  (to be updated 
  #--------------------------------------------- 
  adex$EXTDOSE = as_numeric(adex$EXDOSE )
  adex$EXDOSU = as.character(adex$EXDOSU)
  adex$EXDOSE = as_numeric(adex$EXDOSE)
  ids = which(adex$EXDOSU=="mg/kg")
  adex$EXTDOSE[ids]  = adex$EXDOSE[ids] * adex$WEIGHTBL[ids]   # in "mg" unit
  adex$EXTDOSU = "mg"    # TOTAL DOSE UNIT  
 

  #---------------------------------------------
  # EXDOSE LEVEL:   SUMDOSE  sumation of all of doses (mg) 
  #--------------------------------------------- 
   tdata = adex
   tt =aggregate(as_numeric(tdata$EXTDOSE), by=list(tdata$USUBJID), sum)

   colnames(tt) <- c("USUBJID","SUMDOSE")
   rownames(tt) <- tt$USUBJID
   adex$SUMDOSE = tt[match(adex$USUBJID, rownames(tt)), "SUMDOSE"] 
  

  #---------------------------------------------
  # EXDOSE LEVEL:   CUMSUM  Accumative dose
  #--------------------------------------------- 
   tt = aggregate(as_numeric(adex$EXTDOSE), by=list(adex$USUBJID), cumsum)
   colnames(tt) = c("USUBJID","CUMSUM")
   adex$CUMSUM = 0# unlist(unlist(tt[,"CUMSUM"]))      ### if uniform or list, it will produe an error
   
  
  ############################################################################## 
  ############################################################################## 
  # for nonmem data format
  ############################################################################## 
  ############################################################################## 


  # final output
  key.col.lst = c("STUDYID","USUBJID","ORGID","ARMA","ARMAN","VISITNM","TIMEPT", "NTIM","TIME",
  "EXDOSE","EXDOSU", "EXTDOSE","EXTDOSU","EXSEQ","EXROUTE","EXROUTN","EXDURAT", "EXDINT", "EXDINTN", "SUMDOSE", "CUMSUM")
  
  sec.col.lst = setdiff(colnames(adex),key.col.lst)    # secondary
  col.lst = setdiff(c(key.col.lst, sec.col.lst), colnames(adex))
  if (length(col.lst)>0) {print(paste("mising colnames of ", paste(col.lst, collapse=", ", sep=""), " in adex", sep=""))}
  adex[, col.lst] = ""
  
  adex = adex[, c(key.col.lst )]
  adex = data.frame(adex)  #[, c(key.col.lst, sec.col.lst)])
  rownames(adex) = 1:nrow(adex)
      
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