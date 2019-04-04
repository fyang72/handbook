

#################################################################
# pkmerge
#################################################################
build_adpx <-function(adsl=NULL, adex=NULL, adpc=NULL, other=NULL) {
  
  adsl = fillUpCol_df(adsl, adsl_var_lst)
  adex = fillUpCol_df(adex, nmdat_var_lst)
  adpc = fillUpCol_df(adpc, nmdat_var_lst)
  other = fillUpCol_df(other, nmdat_var_lst)
  adpc = bind_rows(adpc, other)
  
  #------------------
  # prepare for adpc
  #------------------ 
  #   adex  %>% .[[c("USUBJID")]] 
  col_lst = c("USUBJID", "TRTSDTM")
  adpc = adpc %>% select(-TRTSDTM) %>% 
    left_join(adex %>% 
                select(one_of(col_lst)) %>% 
                distinct(USUBJID, .keep_all=TRUE), 
              by="USUBJID")
  
  adpc = adpc %>% mutate(
    TIME = difftime(parse_date_time(SAMDTTM, orders="Ymd HMS", truncated = 3), 
                    parse_date_time(TRTSDTM, orders="Ymd HMS", truncated = 3), 
                    units = "days"
    ) %>% as_numeric()
  )  
  
  #------------------
  # prepare for adex
  #------------------
  # only need USUBJID and other variables that adex do not have, from adsl
  col_lst = c("USUBJID", "WGTBL")
  adex = adex %>% select(-WGTBL)  %>% 
    left_join(adsl %>% 
                distinct(USUBJID, .keep_all=TRUE) %>% 
                select(one_of(col_lst)),
              by="USUBJID")
  
  adex = adex %>% mutate(
    EXTDOSE = ifelse(EXDOSU=="mg/kg", as_numeric(EXDOSE)*as_numeric(WGTBL),  
                     ifelse(EXDOSU=="mg",  as_numeric(EXDOSE), NA)))
  
  #------------------
  # merge to adpx
  #------------------
  # time variable must be character
  adpx = bind_rows(adex[, nmdat_var_lst], adpc[, nmdat_var_lst])  %>% 
    arrange(STUDYID, ARMA, USUBJID, TIME, TEST)
  
  
  #------------------
  # merge to adsl
  #------------------
  col_lst = setdiff(colnames(adsl), c("STUDYID", "USUBJID"))
  adpx = adpx[, setdiff(colnames(adpx), col_lst)]
  
  adpx = adpx %>%  
    left_join(adsl%>%distinct(USUBJID,.keep_all=TRUE), 
              by=c("STUDYID", "USUBJID")
    )
  
  adpx = adpx %>% dplyr::arrange(STUDYID, USUBJID, TIME) 
  
  return(adpx)
}


#################################################################
# pre build_nmdat
#################################################################
build_nmdat0 <-function(dataset) {
  
  # must have these desired variables, if missing, fill with NA 
  nmdat = dataset %>% fillUpCol_df(nmdat_var_lst) 
  
  #---------------------------------------- 
  # Analysis identifiers
  #---------------------------------------- 
  # STUDYID
  nmdat$STUDYID = nmdat$STUDYID 
  
  # USUBJID
  if (all(is.na(nmdat$USUBJID))) {print("error: all(is.na(USUBJID))=TRUE")}
  
  # standardize USUBJID
  nmdat$CLID = nmdat$USUBJID 
  study.id = unique(nmdat$STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  for (i in 1:max(1,length(study.id))) { 
    nmdat$CLID = gsub(study.id[i], "", nmdat$CLID, fix=TRUE)
  }
  nmdat$CLID = gsub("-", "", nmdat$CLID, fix=TRUE)   
  
  t1 = unique(nchar(nmdat$CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)==0)  {print("warning: no USUBJID")
  }else if (length(t1)>1)  {print("warning: the length of CLID in nmdat are not the same.")
  }else{ 
    if (t1==9) { nmdat = nmdat %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
    if (t1==6) { nmdat = nmdat %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
    if (t1==3) { nmdat = nmdat %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
    if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in nmdat !=3, 6 or 9")}
    nmdat$USUBJID <- paste(nmdat$STUDYID,  nmdat$SUBJECT, sep="-") 
    nmdat = nmdat %>% select(-SUBJECT, -CLID)
  }
  
  
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
  
  nmdat$ARMA = nmdat$ARMA
  nmdat$ARMAN = nmdat$ARMAN
  
  #----------------------------------------------------------------------------- 
  # Analysis time variable: "VISIT"  "VISITNUM"  "PCTPT" "NTIM"  "TIME"
  #-----------------------------------------------------------------------------
  nmdat$VISIT = toupper(nmdat$VISIT)  # paste("Visit ", nmdat$VISITNM, sep="") # u.add.prefix(nmdat$VISITNM, prefix="", add.number.zero=3), sep="") 
  nmdat$VISITNUM = extractExpr(nmdat$VISIT, "([0-9]*\\.?[0-9]+)")  %>% as_numeric()
  nmdat$VISIT = paste0("VISIT ", nmdat$VISITNUM)
  
  if (!all(is.na(nmdat$PCTPT)))  {
    nmdat$PCTPT = toupper(nmdat$PCTPT) 
    if (all(is.na(nmdat$NTIM)))  {
      nmdat = nmdat %>% select(-NTIM) %>% 
        left_join(parsePCTPT(nmdat %>% pull(PCTPT) %>% unique()) %>% select(PCTPT, NTIM), by="PCTPT")
    }
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
  if ((!all(class(nmdat$SAMDTTM) %in% c("POSIXct", "POSIXt" )))) { 
    nmdat$SAMDTTM = parse_date_time(nmdat$SAMDTTM, timefmt_var_lst, truncated = 3)  
  } 
  if ((!all(class(nmdat$TRTSDTM) %in% c("POSIXct", "POSIXt" )))) { 
    nmdat$TRTSDTM = parse_date_time(nmdat$TRTSDTM, timefmt_var_lst, truncated = 3)  
  } 
  
  nmdat = nmdat %>% mutate(
    TIME = difftime(SAMDTTM, TRTSDTM, units = "days") %>% as_numeric()
  ) 
  
  #----------------------                  
  # METHOD (SOP), TEST, 
  #---------------------- 
  nmdat <- nmdat %>%  mutate(
    METHOD = METHOD,  
    TEST = TEST, 
    TESTCAT = ordered(TESTCAT, levels=testcat_var_lst), 
    
    DVOR = DVOR,    # keep those ADA results (which is character-based)
    DVORU = tolower(DVORU), 
    
    DVOR = ifelse(DVORU=="ng/ml", as_numeric(DVOR)/1000, DVOR), 
    DVORU = ifelse(DVORU=="ng/ml", "mg/L", DVORU), 
    
    DVORU = ifelse(DVORU=="ug/ml", "mg/L", DVORU), 
    
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
  
  return(nmdat)
}


#################################################################
# build_nmdat1
#################################################################

build_nmdat1 <- function(dataset) {
  
  #---------------------
  # nmdat format specific
  #---------------------
  nmdat = dataset %>% mutate(
    
    # remove PRE-DOSE, default flag
    CFLAG = ifelse(toupper(ARMA) %in% c("PLACEBO"), "Placebo", 
                   ifelse(as_numeric(TIME)<=0, "Predose",  
                          ifelse(as_numeric(TIME)>0 & as.integer(BLQ)==1, "Postdose BLQ", 
                                 as.character(CFLAG)))),
    
    ID    = as.integer(as.factor(USUBJID)),
    ARMA  = ordered(ARMA, levels=unique(ARMA)), 
    ARMAN = as.integer(as.factor(ARMA)),
    TIME  = as_numeric(TIME),
    NTIM  = as_numeric(NTIM),  
    
    AMT  = as_numeric(EXTDOSE), 
    EVID = ifelse(!is.na(AMT)&AMT>0, 1, 0),
    
    RATE = as_numeric(AMT)/as_numeric(EXDUR),      #    "."            "SUBCUTANEOUS" "INTRAVENOUS" 
    #ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), NA,  NA)),
    RATE = ifelse(is.infinite(RATE)|is.na(RATE), NA, RATE), 
    
    CMT = ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), 2,
                 ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), 1, 0)),    
    
    EXROUTE = ordered(EXROUTE, levels = c(route_var_lst)),
    EXROUTN = ifelse(is.na(EXROUTE), -99, as.integer(as.factor(EXROUTE))),
    
    TEST = ordered(TEST, levels= unique(TEST)),   
    
    TESTCAT = ordered(TESTCAT, levels= testcat_var_lst),  
    
    DV = DVOR,  # ifelse(TEST=="REGN1500" & as_numeric(DVOR)!=0, log(as_numeric(DVOR)), DVOR),
    BLQ = ifelse(as_numeric(DVOR)<as_numeric(LLOQ), 1, 0), 
    
    LLOQ = as_numeric(LLOQ), 
    MDV = ifelse(is.na(as_numeric(DV)), 1, 0)
    
  )     
  
  # order it 
  nmdat = nmdat %>% dplyr::arrange(STUDYID, USUBJID, TIME, desc(EVID)) 
  
  #ROWID 
  if ( nrow(nmdat)>0) {nmdat$ROWID=1:nrow(nmdat) }
  nmdat$C[which(!nmdat$CFLAG %in% c(NA,""))] = "C"
  
  # re-order columns
  nmdat = nmdat[, c(nmdat_var_lst, setdiff(colnames(nmdat), nmdat_var_lst))]
  
  return(nmdat)
}


#################################################################
# build_nmdat2
#################################################################

build_nmdat2 <- function(dataset) {
  
  #remove NA, . and empty space
  nmdat <- dataset
  
  nmdat[] <- lapply(nmdat, as.character)
  
  nmdat[is.na(nmdat)] = "." 
  colnames(nmdat) <- gsub(".", "_", colnames(nmdat), fixed=TRUE)
  colnames(nmdat) <- gsub(" ", "_", colnames(nmdat), fixed=TRUE)
  
  # remove "," and " " in some columns 
  for (icol in 1:ncol(nmdat)) {
    nmdat[, icol] = gsub(" ", "_", nmdat[, icol], fixed=TRUE)
    nmdat[, icol] = gsub(",", "_", nmdat[, icol], fixed=TRUE)
  } 
  
  return(nmdat)
}





#################################################################
# check_adex
#################################################################

check_nmdat <- function(dataset, topN=20) {
  nmdat = dataset %>% ungroup()
  table = NULL
  
  #----------------- 
  # TIME
  #----------------- 
  tabl = nmdat %>% select(USUBJID, ARMA, VISIT, PCTPT, TIME, SAMDTTM, TRTSDTM ) %>% 
    filter(is.na(TIME))  %>% 
    arrange(USUBJID)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of elapse time (TIME) that are not avaible. "  
  attr(tabl, "key") = "TIME"  
  attr(tabl, "value") = "TIME"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["TIME"]] = tabl
  
  #----------------- 
  # EXTDOSE
  #----------------- 
  tabl = nmdat %>% select(USUBJID, ARMA, EXTDOSE, EXDOSE, EXDOSU, WGTBL ) %>% 
    filter(is.na(EXTDOSE))  %>% 
    arrange(USUBJID)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of dose (EXTDOSE) that are not avaible. "  
  attr(tabl, "key") = "EXTDOSE"  
  attr(tabl, "value") = "EXTDOSE"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["EXTDOSE"]] = tabl
  
  return(table)
}




#################################################################
# final output
#################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  # nmdat <- build_adpx(adsl, adex, adpc, other)  %>% 
  #   build_nmdat1() %>% 
  #   build_nmdat2() 
  
  nmdat <- build_nmdat0(dataset)  %>% 
    build_nmdat1() %>% 
    build_nmdat2()  
  
  data[["nmdat"]] = nmdat 
  table <- check_nmdat(nmdat, topN=20)   # date_time_format = c("Ymd HMS")
  output <- list(data=data, table=table)
}
