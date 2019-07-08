
########################################################################
# pre build_nmdat, standrization of nmdat, maybe from adpx
########################################################################
build_nmdat0 <-function(dataset) {
  
  #-------------------------------------------------------------  
  # adsl: 
  #   STUDYID, USUBJID, WGTBL
  # adex: 
  #   STUDYID, USUBJID, EXENDTC, EXSTDTC, EXDOSE, EXDOSU, EXROUTE, EXTRT, ARMA
  # adpc: 
  #   STUDYID, USUBJID,	SAMDTTM,	DVOR,	DVORU,	VISIT, 	TIMEPT,	#NTIM,
  #   TEST, TESTCD,	TESTCAT, TESTLABL,	BLQ,	LLOQ,	 METHOD
  #-------------------------------------------------------------
  
  # grouped variables cause lots of troubles
  nmdat <- dataset %>% as.data.frame() %>% ungroup()    
  
  # must have these desired variables, if missing, fill with NA   
  nmdat = nmdat %>% fillUpCol_df(nmdat_var_lst) 
  
  #---------------------------------------- 
  # Analysis identifiers
  #---------------------------------------- 
  # STUDYID
  nmdat$STUDYID = nmdat$STUDYID 
  
  # USUBJID
  if (all(is.na(nmdat$USUBJID))) {print("error: all(is.na(USUBJID))=TRUE")}
  
  # standardize USUBJID
  nmdat <- nmdat %>% mutate(USUBJID = standardise_USUBJID(STUDYID, USUBJID))  
  
  #----------------------------------------------------------------------------- 
  # ARMA LEVEL:   "ARMA" "ARMAN"  
  #----------------------------------------------------------------------------- 
  # http://www.cwladis.com/math104/lecture5.php   
  #  subcultaneously (subQ), intramuscularly (IM), or intravenously (IV).
  #  q.h.     q.2.h.     q.3.h.    q.4.h.
  #  q.d.    (q.o.d.)
  #  b.i.d   twice a day
  #  t.i.d   three times a day
  #  q.i.d   four times a day  
  
  nmdat <- nmdat %>% mutate(
    ARMA = ordered(ARMA, levels=unique(ARMA)), 
    ARMAN = as.integer(ARMA), 
    ARMA = as.character(ARMA)
  )
  
  #----------------------------------------------------------------------------- 
  # Analysis time variable: "VISIT"  "VISITNUM"  "TIMEPT" "NTIM"  "TIME"
  #-----------------------------------------------------------------------------
  nmdat <- nmdat %>% mutate(
    VISIT = toupper(as.character(VISIT)),    
    VISITNUM = extractExpr(VISIT, "([0-9]+)") %>% as.integer(), 
    VISIT = paste0("VISIT ", VISITNUM)
  )
  
  # TIMEPT and NTIM
  adpc$TIMEPT = toupper(adpc$TIMEPT) 
  if (all(is.na(adpc$NTIM)))  {
    adpc = adpc %>% select(-NTIM) %>% 
      left_join(
        parseTIMEPT(adpc %>% pull(TIMEPT) %>% unique()) %>% 
          select(TIMEPT, NTIM), 
        by="TIMEPT")
  }
  
  # TIME
  # -----------------------------------------------  
  #UTC is not a time zone, but a time standard that is the basis for civil time and time zones worldwide.
  #This means that no country or territory officially uses UTC as a local time.
  
  # SAMDTTM:  start Date/time of sampling   TRTSTDTM    TRTSDTM
  #nmdat$SAMDTTM = as.POSIXct(nmdat$EXSTDT*60*60*24 + nmdat$EXSTTM, origin="1960-01-01", tz="GMT")
  #nmdat$SAMDTTM = as.POSIXct(nmdat$TRTSDTM, origin="1960-01-01", tz="GMT") 
  # https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
  
  library(lubridate) 
  nmdat = nmdat %>% mutate( 
    SAMDTTM = parse_date_time(SAMDTTM, orders=timefmt_var_lst, truncated = 3),  
    EXENDTC = parse_date_time(EXENDTC, orders=timefmt_var_lst, truncated = 3),
    EXSTDTC = parse_date_time(EXSTDTC, orders=timefmt_var_lst, truncated = 3) 
  ) %>% 
    group_by(USUBJID) %>%  mutate(TRTSDTM = min(EXSTDTC, na.rm=TRUE)) %>% 
    ungroup()
  
  # TIME
  ids <- which(is.na(as_numeric(nmdat$TIME)))
  nmdat$TIME[ids] = difftime(
    nmdat$SAMDTTM[ids], nmdat$TRTSDTM[ids], units = "days") %>% 
    as_numeric()  
  
  ids <- which(is.na(as_numeric(nmdat$TIME)))
  nmdat$TIME[ids] = difftime(
    nmdat$EXSTDTC[ids], nmdat$TRTSDTM[ids], units = "days") %>% 
    as_numeric()  
  
  # EXDUR
  adex = adex %>% mutate( 
    EXDUR = difftime(EXENDTC, EXSTDTC, units = "days") %>% as_numeric()
  )
  
  # character
  nmdat = nmdat %>% 
    mutate(SAMDTTM = as.character(SAMDTTM), 
           TRTSDTM = as.character(TRTSDTM), 
           EXSTDTC = as.character(EXSTDTC), 
           EXENDTC = as.character(EXENDTC)
    ) 
  
  
  #----------------------                  
  # METHOD (SOP), TEST, 
  #---------------------- 
  nmdat <- nmdat %>%  mutate(
    EXTRT = ordered(EXTRT, levels=unique(EXTRT)), 
    EXTRTN = as.integer(EXTRT), 
    EXTRT = as.character(EXTRT),
    
    METHOD = as.character(METHOD), 
    
    TEST = ordered(TEST, levels=unique(TEST)), 
    TESTN = as.integer(TEST), 
    TEST = as.character(TEST),
    
    TESTCD = as.character(TESTCD),
    TESTCAT = as.character(TESTCAT),
    
    DVOR = as_numeric(DVOR), # keep those ADA results (which is character-based)
    
    DVOR = ifelse(tolower(DVORU)=="ng/ml", as_numeric(DVOR)/1000, DVOR), 
    DVORU = ifelse(tolower(DVORU)=="ng/ml", "mg/L", as.character(DVORU)), 
    
    DVORU = ifelse(tolower(DVORU)=="ug/ml", "mg/L", as.character(DVORU)), 
    
    LLOQ = ifelse(is.na(as_numeric(LLOQ)), NA, as_numeric(LLOQ)),
    BLQ = ifelse(as_numeric(DVOR)<LLOQ, 1, 0) 
    
  )   
  
  #----------------------                  
  # dosing events
  #---------------------- 
  # col.lst = c("USUBJID", "WGTBL")
  # nmdat = nmdat %>% select(-WGTBL)  %>% 
  #   left_join(adsl %>% distinct(USUBJID, .keep_all=TRUE) %>% 
  #               select(one_of(col.lst)),
  #             by="USUBJID")
  
  nmdat = nmdat %>% mutate(
    EXTDOSE = ifelse(EXDOSU=="mg/kg", as_numeric(EXDOSE)*as_numeric(WGTBL),  
                     ifelse(EXDOSU=="mg",  as_numeric(EXDOSE), NA)))
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------     
  nmdat = nmdat[, c(nmdat_var_lst, setdiff(colnames(nmdat), nmdat_var_lst))]
  nmdat <- convert_vars_type(nmdat, nmdat_var_lst)
  nmdat <- nmdat %>% dplyr::arrange(STUDYID, USUBJID, TIME, TESTN) 
  nmdat <- nmdat %>% ungroup()
  
  return(nmdat)
}
