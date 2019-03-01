



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

 

build_adpc <- function(adpc, adsl=NULL, adex=NULL, INFHR=1, CYCLE_DURATION=56, LLOQ, NTIM_MANUAL) {
   
  # SOP = c("PCL3671", "PCL3679")
  # LLOQ = c(0.078, 0.0195)
  # LLOQ = data.frame(SOP, LLOQ)
  # LLOQ
  # 
  # TIMEPT = c("SCREENING", "EOS", "ET")
  # NTIM = c(-14, 112, 88)
  # NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  # NTIM_MANUAL
  
  # colnames(adpc) from Deli
  colnames(adpc) = toupper(colnames(adpc))
  adpc.col.lst <- c(
    "STUDYID", "CLID", "VISIT", "TIMEPT", "SAMDTTM",                      
    "SOP", "RESC", "RESC_RAW", "STDUNIT", "BLQ", "TEST",           
    "SAMPCOND", "SAMPTYPE", "SAMPDES", "QCQA1",  "QCQA2",               
    "DRUG1",  "DOSEPLAN_LIM",   "DOSU", "ROUTE","TRTGROUP","TREATMENT", "DOSING_INTERVAL1",
    "PLATE_TYPE",   "PCLSPECCONF"               
  )  
  
  adpc = adpc %>% mutate(DRUG1=ifelse(!is.null(DRUG1), DRUG1, 
                                      ifelse(!is.null(DRUG), DRUG, "")),
                         DOSEPLAN_LIM=ifelse(!is.null(DOSEPLAN_LIM), DOSEPLAN_LIM, 
                                             ifelse(!is.null(DOSEPLAN), DOSEPLAN, "")), 
                         DOSING_INTERVAL1=ifelse(!is.null(DOSING_INTERVAL1), DOSING_INTERVAL1, 
                                                 ifelse(!is.null(DOSING_INTERVAL), DOSING_INTERVAL, ""))
  ) 
  
  col.lst = setdiff(adpc.col.lst, colnames(adpc))
  if (length(col.lst)>0) {print(paste0("Missing key column(s) of ", paste(col.lst, collapse=", ")))}
  
  
  #----------------------------------------------------------------------------- 
  # Default check
  #-----------------------------------------------------------------------------  
  adpc <- data.frame(adpc, stringsAsFactors=FALSE)     
  colnames(adpc) <- toupper(colnames(adpc))
  
  
  #----------------------------------------------------------------------------- 
  # SUBJECT LEVEL: "STUDYID"  "USUBJID"  "SUBJECT"  "SUBJECTN"
  #-----------------------------------------------------------------------------
  adpc$STUDYID = adpc$STUDYID 
  
  adpc$CLID = gsub("-", "", adpc$CLID, fix=TRUE)
  adpc$CLID = gsub("_", "", adpc$CLID, fix=TRUE)
  
  t1 = unique(nchar(adpc$CLID))
  if (length(t1)>1)  {print("CLID in adpc have irregular ID --length(unique(nchar(adpc$CLID)))>1")}
  if (t1==9) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
  if (t1==6) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
  if (t1==3) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
  if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adpc !=3, 6 or 9")}
  
  adpc$USUBJID <- paste(adpc$STUDYID,  adpc$SUBJECT, sep="-") 
  adpc$SUBJECTN <- as_numeric(gsub("-","", adpc$SUBJECT))  
  
  
  #----------------------------------------------------------------------------- 
  # ARMA LEVEL: "GROUP","GROUPN", "ARMA" "ARMAN" "COHORT" "COHORTN" "TRT" "TRTN" "PHASE"  "CYCLE"  
  #----------------------------------------------------------------------------- 
  if (!is.null(adex)) {
    adpc = left_join(adpc, adex%>%distinct(USUBJID, .keep_all=TRUE)%>%select(USUBJID, ARMA), by="USUBJID") # getting actual dosing arm
  }else {
    adpc = adpc %>% mutate(ARMA = paste(DRUG1, DOSEPLAN_LIM, DOSU, ROUTE, DOSING_INTERVAL1, sep=" "))
  }
  
  
  #----------------------------------------------------------------------------- 
  # TIME LEVEL: "VISIT"    "VISITNM"  "TIMEPT"   "NTIM/NOM_DAY"  "TIME/PKDAY_C"
  #-----------------------------------------------------------------------------
  
  # VISIT     (VISIT-997:  corresponds to ET)
  # -----------------------------------------------
  adpc$VISITNM = as_numeric(gsub("VISIT", "", toupper(adpc$VISIT), fix=TRUE))    #  or adpc$VISITNUM
  adpc$VISIT = paste("Visit ", adpc$VISITNM, sep="") # u.add.prefix(adpc$VISITNM, prefix="", add.number.zero=3), sep="") 
  as.matrix(unique(adpc$VISIT))
  
  # TIMEPT
  # SCREENING
  # DAY 1
  # DAY 1 PRE  :       DAY 1 PRE-DOSE
  # DAY 1 POST:        DAY 1 0HR, end of infusion
  # DAY 29 PRE: 
  # DAY 29 POST:       DAY 29 0HR
  # EOS:               END OF STUDY
  # ET:                EARLY TERMINATION   VISIT=997
  # ----------------------------------------------- 
  as.matrix(unique(adpc$TIMEPT))
  adpc$TIMEPT = gsub("POST", "0HR", adpc$TIMEPT, fix=TRUE)
  adpc$TIMEPT = gsub("Post", "0HR", adpc$TIMEPT, fix=TRUE)
  adpc$TIMEPT = gsub("post", "0HR", adpc$TIMEPT, fix=TRUE)
  adpc$TIMEPT = gsub("EOI", "0HR", adpc$TIMEPT, fix=TRUE)
  adpc$TIMEPT = gsub("eoi", "0HR", adpc$TIMEPT, fix=TRUE)
  
  
  
  # NOM_DAY, NTIM :  
  # -----------------------------------------------
  # using manual input, saved as NTIM0
  NTIM_MANUAL = NTIM_MANUAL %>% mutate(TIMEPT=as.character(TIMEPT), NTIM0=as.character(NTIM))
  adpc = adpc %>% left_join(NTIM_MANUAL %>% select(TIMEPT, NTIM0), by="TIMEPT")
  
  
  NTIM=separate_strVec2matrix(adpc$TIMEPT, sep=" ")
  if (ncol(NTIM)==2) {NTIM$V3 = "NA"}
  colnames(NTIM)[1:3] = c("DAY", "NDAY", "HR_STR")   #"DAY   1    1HR" 
  NTIM = NTIM %>% mutate(NDAY=as_numeric(NDAY)) %>%
    mutate(NDAY=ifelse(DAY=="Week"|DAY=="WEEK", NDAY*7, NDAY))
  
  
  HOUR = data.frame(HR = seq(0, 24, by=1), 
                    HR_STR=c("PRE", paste0(seq(1, 24, by=1), "HR"))) %>%
    mutate(HR_STR=as.character(HR_STR))
  NTIM = left_join(NTIM, HOUR, by="HR_STR")  
  
  # INFHR = 1  # INFUSION HOUR
  NTIM = NTIM %>% mutate(HR=ifelse(is.na(HR), 0, HR)) %>% 
    mutate(HR=ifelse(!HR_STR %in% c("PRE", "NA"), HR+INFHR, HR)) %>% 
    mutate(NDAY=as_numeric(NDAY)) %>%
    mutate(NDAY=ifelse(NDAY>0, as_numeric(NDAY)-1, NDAY)) %>%
    mutate(NTIM=round(NDAY+HR/24, digits=3))
  
  head(NTIM, n=25)
  adpc = adpc %>% bind_cols(NTIM%>%select(NDAY,NTIM))
  
  
  
  # if parse failed, then use manual interpretation [NTIM0] provided by users
  adpc = adpc %>% mutate(NTIM=ifelse(is.na(NTIM), NTIM0, NTIM)) %>%
    mutate(NTIM=as_numeric(NTIM))
  
  # list of iregular TIMEPT, such as VISIT 998, TIMEPT=ET, etc.
  as.matrix(unique(adpc$NTIM))
  col.lst = c("VISIT","VISITNM","TIMEPT","NDAY", "NTIM", "NTIM0")
  adpc[which(is.na(adpc$NTIM)), col.lst]
  
  #adpc %>% select(CYCLE, VISIT, NTIM, TIMEPT) %>% head()
  # if there is a cycle for Oncology programs
  #adpc$NTIM = (as_numeric(adpc$CYCLE)-1)*CYCLE_DURATION + adpc$NTIM   
  
  
  # PKDAY_C
  # -----------------------------------------------  
  #But GMT is a time zone and UTC is a time standard.
  #Although GMT and UTC share the same current time in practice, there is a basic difference between the two:
  
  #  GMT is a time zone officially used in some European and African countries. The time can be displayed using 
  # both the 24-hour format (0 - 24) or the 12-hour format (1 - 12 am/pm).
  
  #UTC is not a time zone, but a time standard that is the basis for civil time and time zones worldwide. This means 
  # that no country or territory officially uses UTC as a local time.
  
  # SAMDTTM:  start Date/time of sampling   TRTSTDTM    TRTSDTM
  #adex$SAMDTTM = as.POSIXct(adex$EXSTDT*60*60*24 + adex$EXSTTM, origin="1960-01-01", tz="GMT")
  #adex$SAMDTTM = as.POSIXct(adex$TRTSDTM, origin="1960-01-01", tz="GMT")
  #adex$SAMDTTM = as.POSIXlt(paste(DATE, "T", TIME1, sep=""), format = "%Y-%m-%dT%H:%M", tz="GMT")
  #strptime("Tue, 23 Mar 2010 14:36:38 -0400",  "%a, %d %b %Y %H:%M:%S %z")  
  adpc$SAMDTTM = as.POSIXct(adpc$SAMDTTM, origin="1960-01-01", tz="UTC")   # Coordinated Universal Time or UTC
  
  if (is.null(adex)) {adpc$TIME = adpc$NTIM;    # default using NTIM as TIME
  }else if ("TRTSTDTM" %in% colnames(adex)) {
    adpc = adpc %>% left_join(adex %>% distinct(USUBJID, .keep_all=TRUE) %>% select(USUBJID, TRTSTDTM), by="USUBJID")
    adpc$TRTSTDTM = as.POSIXct(adpc$TRTSTDTM, origin="1960-01-01", tz="UTC") 
    adpc$TIME = as_numeric(difftime(adpc$SAMDTTM, adpc$TRTSTDTM, units="days"))
  }else {
    adpc$TIME = adpc$NTIM;
  }
  
  adpc$SAMDTTM = as.character(adpc$SAMDTTM); 
  #adpc$ISAMDTTM = as.character(adpc$SAMDTTM); 
  
  
  # SAMPLE LEVEL: "TEST"  "TESTN"  "TESTCD" "SOP"  "CONCCH" "CONCN"  "CONCNI"   
  # "STDUNIT"  "BLQ"  "LLOQ"  "SCAT"  
  #-----------------------------------------------------------------------------                    
  # [1,] "PCL3192"                    # Total REGN1500
  # [5,] "PCL4148"                    # Total ANGPTL3
  
  # [2,] "PCL4015 Confirmation"       
  # [3,] "PCL4015 Screen"            
  # [4,] "PCL4015 Screen Disposition"   
  # [6,] "PCL4318 Confirmation"      
  # [7,] "PCL4318 Screen"            
  # [8,] "PCL4318 Screen Disposition"
  # [9,] "PCL4318 Titer"  
  
  # SOP
  #----------
  adpc$SOP = adpc$SOP
  as.matrix(sort(unique(adpc$SOP))) 
  
  # TEST  
  #---------   
  adpc$TEST = paste(adpc$TEST, adpc$SOP, sep="-")  # [which(adpc$SOP=="PCL3950")] = "REGN2810" 
  if(is.null(adpc$TESTCD)) {adpc$TESTCD = adpc$TEST}
  if(is.null(adpc$TESTN)) {adpc$TESTN = "."}
  as.matrix(sort(unique(adpc$TEST))) 
  
  # DVOR
  #--------- 
  ids = which(!adpc$SAMPDES %in% c("Ab" ))    # "Ab"
  adpc$RESC_RAW[ids] = as_numeric(adpc$RESC_RAW[ids])/1000   # all test results convert to mg/L, if not Ab
  adpc$CONCN = adpc$RESC_RAW    # used for adpc
  adpc$DVOR = adpc$RESC_RAW  # try to align with adpx
  #adpc = adpc %>% mutate(DVOR=ifelse(is.na(DVOR), RESC_RAW, DVOR))
  
  adpc$STDUNIT = as.character(adpc$STDUNIT)
  
  
  
  # CONCCH and CONCNI
  #----------------------    
  require('Hmisc') 
  # 1g=10^3mg=10^6ug=10^9ng=10^12pg
  
  # get LLOQ for each SOP
  adpc = adpc%>%mutate(SOP=as.character(SOP)) %>% 
    left_join(LLOQ%>%mutate(SOP=as.character(SOP)), by="SOP")
  adpc = adpc %>% mutate(BLQ = ifelse(CONCN<LLOQ, "BLQ", ""))     # need update LIMS data if necessary, $BLQ = as.character(adpc$BLQ)
  
  adpc$CONCCH = adpc$CONCN
  adpc$CONCNI = adpc$CONCN
  
  ids <- which(adpc$SAMPDES!="Ab" &  adpc$CONCN <= adpc$LLOQ)      #################
  adpc$CONCCH[ids] <-  paste0("<", adpc$LLOQ[ids])   # format.pval(adpc$CONCN[ids],eps=LLOQ.DRUG)    #
  adpc$CONCNI[ids] <- adpc$LLOQ[ids]/2;   # imputed concentration 
  
  # SAMPTYPE     
  #---------------------- 
  adpc$SAMPTYPE = adpc$SAMPTYPE       # "Hu Serum"
  
  #----------------------------------------------------------------------------- 
  # final output of the standardized adpc
  #----------------------------------------------------------------------------- 
  #adpc = adpc %>% arrange(USUBJID, NTIM)  
  
  # check all DVOR-related !!!!!!!!!                     
  key.col.lst = c("STUDYID", "USUBJID", "SUBJECT",   "SUBJECTN", "ARMA", "VISIT", "VISITNM", "TIMEPT","NDAY", "NTIM", "TIME", "SAMDTTM", "SOP", "TEST", "DVOR", "CONCN",  "CONCCH", "CONCNI", "STDUNIT", "BLQ", "LLOQ", "QCQA1", "QCQA2")
  col.lst = c(key.col.lst, setdiff(colnames(adpc), key.col.lst))
  adpc = adpc[, col.lst]
  
  #head(adpc[, col.lst], n= 25) %>% as.data.frame()
  
  return(adpc)
  
}



# for debug
if (1==2) {
  
  
  ####pk_r2222_1326_raw.sas7bdat
  filename= "H:\\FYANG\\R2222 RSV\\R2222_HV_1326\\DATA\\PK_0325_2015_FINAL\\pk_r2222_1326_raw.sas7bdat"
  adpc = read_sas(filename)
  unique(adpc$SOP)
  
  SOP = c("PCL3671", "PCL3679")
  LLOQ = c(0.078, 0.0195)
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCREENING", "EOS", "ET")
  NTIM = c(-14, 112, 88)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  ####pk_r1500_1214_raw.sas7bdat
  filename= "H:\\FYANG\\R1500_AngPTL3_Evinacumab\\R1500_HV_1214\\DATA\\from_Kevin_0318_2016\\pk_r1500_1214_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL4148", "PCL3192"    )
  LLOQ = c(0.078, 0.0195)
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCREENING", "EOS", "ET", "U1")
  NTIM = c(-14, 112, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  
  ####pk_r1500_1214_raw.sas7bdat
  filename= "H:\\FYANG\\R1500_AngPTL3_Evinacumab\\R1500_CL_1321\\DATA\\FINAL\\pk_r1500_1321_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL4148", "PCL3192"    )
  LLOQ = c(0.078, 0.0195)
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCREENING", "EOS", "ET", "U1")
  NTIM = c(-14, 112, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  
  
  ####pk_r1500_1214_raw.sas7bdat
  filename= "H:\\FYANG\\R1500_AngPTL3_Evinacumab\\R1500_CL_1331\\DATA\\pk_r1500_1331_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL4148", "PCL3192"    )
  LLOQ = c(0.078, 0.0195)
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCREENING", "EOS", "ET", "U1")
  NTIM = c(-14, 112, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  ####
  
  ####pk_r2222_1332_raw.sas7bdat
  filename= "H:\\FYANG\\R2222 RSV\\R2222_RSV_1332\\DATA\\PART B\\pk_r2222_1332_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL3671" )
  LLOQ = c(0.078 )
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCR", "EOS", "ET", "U1", "U2", "U3")
  NTIM = c(-14, 150, 44, 77, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  
  ####pk_r2222_1332_raw.sas7bdat
  filename= "H:\\FYANG\\R2810_PD1\\DATA\\0911_2017\\pk_r2810_1423_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL3950" )
  LLOQ = c(0.078 )
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCR", "EOS", "ET", "U1", "U2", "U3")
  NTIM = c(-14, 150, 44, 77, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  
  
  ####pk_r2222_1332_raw.sas7bdat
  filename= "H:\\FYANG\\R2810_PD1\\DATA\\0911_2017\\pk_r2810_1540_raw.sas7bdat"
  adpc = read_sas(filename)
  
  unique(adpc$SOP)
  SOP = c("PCL3950" )
  LLOQ = c(0.078 )
  LLOQ = data.frame(SOP, LLOQ)
  LLOQ
  
  unique(adpc$TIMEPT)
  TIMEPT = c("SCR", "EOS", "ET", "U1", "U2", "U3")
  NTIM = c(-14, 400, 44, 77, 88, 99)
  NTIM_MANUAL = data.frame(TIMEPT, NTIM)
  
  
  
  
  
  adpc.v0 = adpc
  adpc = process_adpc(adpc.v0, adsl=NULL, adex=NULL, INFHR=1, LLOQ, IREG_NTIM)
  head(adpc, n=20) %>% as.data.frame()
  
}






