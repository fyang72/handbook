
check_adpx <- function(adpx)   {
  
  #-------------------------------------------------
  # clean adpx for use in nonmem dataset
  #-------------------------------------------------
  
  adpx = adpx     # create an cleaned adpx
  
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
    minimal.col.lst = c("USUBJID", "ARMA","ARMAN","ID", "TIME", "TEST","DVOR", "DV","MDV","CMT","AMT","RATE", "EVID", "EXSEQ","EXROUTN")
    adpx = adpx[, minimal.col.lst]
  }
  
  #-----------------------------------------------------   
  # remove explicit placebo subject  
  #----------------------------------------------------- 
  #adpx$ARMA = toupper(adpx$ARMA)
  
  subj.lst = unique(adpx$USUBJID[which(toupper(adpx$ARMA) %in% c("PLACEBO"))])
  if (length(subj.lst)>0) {print(subj.lst)  }
  
  tdata <- adpx %>% filter(toupper(ARMA)=="PLACEBO")     # adpx[which(!adpx$USUBJID %in% subj.lst),]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to explicit placebo subject",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "PLACEBO.SUBJECT", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  #print("remove explicit placebo subject")
  #print(dim(adpx))
  
  
  #-----------------------------------------------------
  # remove implicit placebo subject, we may suspect such subjects may be dosed with placebo
  #-----------------------------------------------------
  adpx$DVOR =  as_numeric(adpx$DVOR) 
  tdata = adpx %>% filter(EVID==0) %>% group_by(USUBJID) %>% summarise(SUSPECT_SUBJ = all(DVOR %in% c(0, NA)))   # 
  which(tdata$SUSPECT_SUBJ)
  
  subj.lst = tdata %>% filter(!SUSPECT_SUBJ) %>% select(USUBJID) %>% .[[1]]
  subj.lst = setdiff(unique(adpx$USUBJID), subj.lst)
  tdata = adpx %>% filter(USUBJID %in% subj.lst )  %>% select(ROWID, USUBJID, DV, TIME, DVOR)
  
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to implicit placebo subject",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "IMPLICIT.PLACEBO.SUBJECT", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  #print("remove implicit placebo subject")
  #print(dim(adpx))
  
  #-----------------------------------------------------
  # check TIME....
  #-----------------------------------------------------
  adpx = adpx
  adpx$TIME =  as_numeric(adpx$TIME)  
  
  # checking TIME in adpx, must not be "NA" or ".", 
  tdata = adpx[which(is.na(adpx$TIME)),  ]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to is.na(TIME)",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "missing.time", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  
  # checking TIME in adpx, must be >=0
  tdata = adpx[which(adpx$TIME<0 | (adpx$TIME==0&adpx$EVID==0)),  ]
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to TIME<=0",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "Predose.samples", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  # # checking DVOR=0, TIME==0, EVID==0  
  # tdata = adpx[which(adpx$TIME==0 & adpx$DVOR==0 & adpx$TIME==0),  ]
  # if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to DVOR=0,TIME=0,EVID=0",sep=""))
  # adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "TIME.LE.ZERO", CFLAG), 
  #                        C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  # ) 
  
  # records must start the first dose, i.e. EVID=1, TIME=0 and EXSEQ=1
  tdata = adpx[which(adpx$TIME==0 & adpx$EVID==1 & adpx$EXSEQ==1),  ]      # R2810-ONC-1423-840-003-011    R2810-ONC-1423-840-008-001  why EXSEQ=0
  #print(tdata)
  
  adpx[which(adpx$USUBJID %in% setdiff(adpx$USUBJID, tdata$USUBJID)), ]   
  
  adpx = adpx
  
  #print("check TIME placebo subject")
  #print(dim(adpx))
  #-----------------------------------------------------
  # for a given ID, TEST, TIME, any duplicated records?
  #-----------------------------------------------------
  
  
  
  #-----------------------------------------------------
  # check unrealistic peak : trough ratios in DVOR
  #-----------------------------------------------------
  if (1==2 ) {
    adpx = adpx
    adpx$TIME = as_numeric(adpx$TIME)
    adpx$DVOR = as_numeric(adpx$DVOR)  
    
    # only concern about PRE and 0HR
    ids =  regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0      
    adpx = adpx[ids, ]
    
    adpx$DVOR = as_numeric(adpx$DVOR)      
    adpx$EVENT = NA            
    adpx$EVENT[which(regexpr('PRE', adpx[, "TIMEPT"]) >=0)] = "PRE"
    adpx$EVENT[which(regexpr('0HR', adpx[, "TIMEPT"]) >=0)] = "POST"
    
    # extract the DVOR for POST and PRE, and calculate the RATIO
    tt = acast(adpx, USUBJID+ARMA+EXSEQ ~ EVENT, fun.aggregate = fun.mean, value.var="DVOR")
    tt = data.frame(colsplit(rownames(tt),"_",c("USUBJID","ARMA","EXSEQ")), tt)
    tt = tt[order(tt$USUBJID, tt$EXSEQ),]
    rownames(tt) = 1:nrow(tt)
    tt$PRE = as_numeric(tt$PRE)
    tt$POST = as_numeric(tt$POST)  
    tt$PRE[which(tt$PRE==0)] = 0.078
    tt$POST[which(tt$POST==0)] = 0.078
    tt$RATIO = as_numeric(tt$POST)/as_numeric(tt$PRE) 
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
    adpx = adpx
    ids = adpx$EVID==0 & 
      (regexpr('PRE', adpx$TIMEPT) >=0 | regexpr('0HR', adpx$TIMEPT) >=0) &          
      paste(adpx$USUBJID,adpx$EXSEQ,sep="-") %in% paste(tt2$USUBJID,tt2$EXSEQ,sep="-")  
    adpx[ids, adpx.col.lst]
    
    tdata = adpx[!ids, ]
    if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to abnormal peak:trough ratio",sep=""))
    adpx = tdata
    
    adpx = adpx
  } 
  
  
  
  #-----------------------------------------------------
  # AMT can't be zero
  #-----------------------------------------------------
  
  tdata = adpx %>% mutate(AMT=as_numeric(AMT)) %>% filter(EVID==1, AMT %in% c(0, NA))
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to AMT.IS.ZERO",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "AMT.IS.ZERO", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  # print("AMT can't be zero")
  # print(dim(adpx))
  
  
  #-----------------------------------------------------
  # no dosing event, but having concentration, should removed. 
  #-----------------------------------------------------
  adpx = adpx
  
  tdata = adpx  
  tdata$AMT = as_numeric(tdata$AMT)
  tdata$DVOR = as_numeric(tdata$DVOR)  
  subj.adex = unique(tdata[which(tdata$EVID==1 & tdata$AMT!=0),"USUBJID"] )     # subjects having valid dosing
  subj.adpc = unique(tdata[which(tdata$EVID==0 & !is.na(tdata$DVOR)),"USUBJID"] )   # subject having valid conc
  
  # identify subjects who have adpc information but no dosing information                                               
  subj.lst = setdiff(subj.adpc, subj.adex)
  tdata = filter(adpx, (USUBJID %in% subj.lst)) 
  if(nrow(tdata)>0) print(paste(nrow(tdata), " of records has been removed due to having adpc information but no dosing information ",sep=""))
  adpx = adpx %>% mutate(CFLAG=ifelse(ROWID %in% tdata$ROWID, "NO.DOSING.INFO", CFLAG), 
                         C=ifelse(ROWID %in% tdata$ROWID, "C", C)
  ) 
  
  #print("no dosing event, but having concentration, should removed. ")
  #print(dim(adpx))
  
  
  #-----------------------------------------------------
  # whether or not remove all DV = 0 or BLQ data entries;
  #----------------------------------------------------- 
  adpx = adpx
  
  if (1==2) {
    ids <- (as_numeric(adpx$DVOR) == 0 | is.na(as_numeric(adpx$DVOR)))  &  
      as_numeric(adpx$EVID) == 0 
    #adpx[ids, "DVOR"] = ".";     adpx[ids, "MDV"] = 1
    
    tdata = adpx[!ids, ]      #  adpx[ids, adpx.col.lst]
    if(nrow(adpx)!=nrow(tdata)) print(paste(nrow(adpx)-nrow(tdata), " of records has been removed due to BLQ",sep=""))
    adpx = tdata
    
    adpx = adpx
  }
  
  
  return(adpx)
}

