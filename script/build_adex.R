

#################################################################
# build_adex
#################################################################
build_adex <-function(dataset) {
  
  # must have these desired variables, if missing, fill with NA 
  adex = dataset %>% fillUpCol_df(adex_var_lst) 
    
  
  #---------------------------------------- 
  # Analysis identifiers
  #---------------------------------------- 
  # STUDYID
  adex$STUDYID = adex$STUDYID 
  
  # USUBJID
  if (all(is.na(adex$USUBJID))) {print("error: all(is.na(USUBJID))=TRUE")}
  
  # standardize USUBJID
  adex$CLID = adex$USUBJID 
  study.id = unique(adex$STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  for (i in 1:max(1,length(study.id))) { 
    adex$CLID = gsub(study.id[i], "", adex$CLID, fix=TRUE)
  }
  adex$CLID = gsub("-", "", adex$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adex$CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)==0)  {print("warning: no USUBJID")
  }else if (length(t1)>1)  {print("warning: the length of CLID in adex are not the same.")
  }else{ 
    if (t1==9) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
    if (t1==6) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
    if (t1==3) { adex = adex %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
    if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adex !=3, 6 or 9")}
    adex$USUBJID <- paste(adex$STUDYID,  adex$SUBJECT, sep="-") 
    adex = adex %>% select(-SUBJECT, -CLID)
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
   
  adex$ARMA = adex$ARMA
  adex$ARMAN = adex$ARMAN
  
  
  #---------------------------------------------
  # dosing variable: "EXSTDTC",  "EXENDTC", "EXDUR", "TRTSDTM", "TIME"
  #--------------------------------------------- 
  # https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time
  # "ymd"   "09-01-03 12:02"  "09-01-01", "090102", "09-01 03"
  # "ymd HM"  "09-01-03 12:02"  
  # x <- c("2011-12-31 12:59:59", "2010-01-01 12:11", "2010-01-01 12", "2010-01-01")
  # parse_date_time(x, "Ymd HMS", truncated = 3)
  #UTC is n ot a time zone, but a time standard that is the basis for civil time and time zones worldwide. This means 
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
  
  library(lubridate) 
 
  adex = adex %>% mutate( 
    EXENDTC = parse_date_time(EXENDTC, timefmt_var_lst, truncated = 3),
    EXSTDTC = parse_date_time(EXSTDTC, timefmt_var_lst, truncated = 3) 
  ) %>% 
    group_by(USUBJID) %>% 
      mutate(TRTSDTM = min(EXSTDTC))  
  
  # "TRTSDT"   "TRTSTM"   "TRTSDTM"    date/time of start treatment 
  # "TRTEDT"   "TRTETM"   "TRTEDTM"    date/time of end of treatment   
  adex = adex %>% mutate( 
    EXDUR = difftime(EXENDTC, EXSTDTC, units = "days") %>% as_numeric(), 
    TIME = difftime(EXSTDTC, TRTSDTM, units = "days") %>% as_numeric()
  )
  
  adex = adex %>% 
    mutate(EXENDTC = as.character(EXENDTC), 
           EXSTDTC = as.character(EXSTDTC), 
           TRTSDTM = as.character(TRTSDTM) 
    ) %>% ungroup()
    
  
  #---------------------------------------------
  # "EXROUTE"    "SUBCUTANEOUS"  "INTRAVENOUS"   "INTRAMUSCULAR" "IVT"      
  #--------------------------------------------- 
  adex = adex %>% mutate(EXROUTE = ordered(toupper(EXROUTE), levels = route_var_lst),  
                         EXROUTN = ifelse(is.na(EXROUTE), -99, as.integer(EXROUTE)), 
                         
                         EXDOSE = as_numeric(EXDOSE), 
                         EXDOSU = ordered(EXDOSU, levels = dosu_var_lst)  
  )
  

  
  #---------------------------------------------
  # EXSEQ
  #--------------------------------------------- 
  adex = adex %>% group_by(USUBJID) %>% 
    mutate(EVID = ifelse(as_numeric(EXDOSE)>0, 1, 0), 
           EXSEQ = cumsum(EVID)
           ) %>% 
    select(-EVID)
  
  # adex %>% select(USUBJID, EXSTDTC, EXENDTC, EXDUR, TRTSDTM, TIME, EXSEQ)
  
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  col.lst = c(adex_var_lst, setdiff(colnames(adex), adex_var_lst))
  adex = adex[, col.lst]  %>% arrange(STUDYID, USUBJID, TIME)
  
  return(adex) 
}


#################################################################
# check_adex
#################################################################

check_adex <- function(dataset, adex, topN=20) { 
  
  dataset = dataset %>% ungroup()  %>% 
    rename_at(vars(colnames(dataset)),
              ~ paste0(colnames(dataset), "_ORG")
    )
  adex = bind_cols(adex, dataset)
  
  table = NULL
  
  #----------------- 
  # TRTSDTM
  #-----------------
  tabl = adex %>% select(USUBJID, TRTSDTM) %>% 
    distinct(USUBJID, .keep_all=TRUE) %>% 
    filter(is.na(TRTSDTM)) %>%  
    arrange(USUBJID)  
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of subject (USUBJID) whose starting dosing time (TRTSDTM) can't be determined" 
  attr(tabl, "key") = "USUBJID"
  attr(tabl, "value") = "TRTSDTM"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["TRTSDTM"]] = tabl
  
  #----------------- 
  # ARMA
  #-----------------
  tabl = adex %>% select(ARMA, ARMAN, ARMA_ORG) %>% distinct(ARMA, .keep_all=TRUE) %>% 
    arrange(ARMA)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of treatment arm (ARMA) and its order (ARMAN)"  
  attr(tabl, "key") = "ARMA_ORG"  
  attr(tabl, "value") = "ARMAN"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["ARMA"]] = tabl
  
  #----------------- 
  # EXROUTE
  #-----------------  
  tabl = adex %>% select(EXROUTE, EXROUTE_ORG) %>% 
    distinct(EXROUTE, .keep_all=TRUE) %>% 
    arrange(EXROUTE)
  
  #tabl = tabl %>% mutate(SUGGEST=ordered(NA, levels=route_var_lst))
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of administration route (EXROUTE) and its order (EXROUTN)"  
  attr(tabl, "key") = "EXROUTE_ORG"  
  attr(tabl, "value") = "EXROUTN"
  
  attr(tabl, "footnote") = paste0("Note, the standarad dose administration routes are ", paste0(route_var_lst, collapse=", "), ". ")  
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0(attr(tabl, "footnote"), "The default is to display top ", topN, " rows.")}
  table[["EXROUTE"]] = tabl
  
  
  #----------------- 
  # EXDOSU
  #-----------------  
  tabl = adex %>% select(EXDOSU, EXDOSU_ORG) %>% 
    distinct(EXDOSU, .keep_all=TRUE) %>% 
    arrange(EXDOSU)
  
  #tabl = tabl %>% mutate(SUGGEST=ordered(NA, levels=dosu_var_lst))
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of dose unit (EXDOSU)"  
  attr(tabl, "key") = "EXDOSU_ORG"  
  attr(tabl, "value") = "EXDOSU"
  
  attr(tabl, "footnote") = paste0("Note, the standarad dose units are ", paste0(dosu_var_lst, collapse=", "), ". ")
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0(attr(tabl, "footnote"), 
                                                        "The default is to display top ", topN, " rows.")}
  table[["EXDOSU"]] = tabl
  
  
  return(table)
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adex <- build_adex(dataset, 
                    timefmt_var_lst = timefmt_var_lst, 
                    dosu_var_lst = dosu_var_lst, 
                    route_var_lst = route_var_lst
                    )    
  
  data[["adex"]] = adex 
  table <- check_adex(dataset, adex, topN=topN)    
  output <- list(data=data, table=table)
}
