
########################################################################
# pkmerge from adsl, adex, adpc, and others
########################################################################
build_adpx <-function(adsl=NULL, adex=NULL, adpc=NULL, other=NULL) {
  
  validate(need(adsl, message="no adsl in build_adpx"),
           need(adex, message="no adsl in build_adex"), 
           need(adpc, message="no adsl in build_adpc")
           )
  # toupper
  colnames(adsl) <- toupper(colnames(adsl))
  colnames(adex) <- toupper(colnames(adex))
  colnames(adpc) <- toupper(colnames(adpc))
 
  
  # adsl
  adsl = adsl %>% ungroup() %>% fillUpCol_df(adsl_var_lst)
  
  # adex
  col_lst <- c(nmdat_var_lst, 
               colnames(adex), 
               colnames(adpc), 
               colnames(other)
               ) %>% unique()
  
  # expand to col_lst
  adex = adex %>% ungroup() %>% fillUpCol_df(col_lst)
  adpc = adpc %>% ungroup() %>% fillUpCol_df(col_lst)
  
  # adpc = adpc + other (convert to characters)
  if (!is.null(other)) {
    colnames(other) <- toupper(colnames(other))
    other = other %>% ungroup() %>% fillUpCol_df(col_lst) 
    
    #w <- which(sapply(other, function(x)  tail(class(x),1)) %in% c('factor', 'POSIXt', 'POSIXlt'))
    other <- lapply(other, function(x) as.character(x) ) 
    adpc <- lapply(adpc, function(x) as.character(x) )
    
    adpc = bind_rows(adpc[, col_lst], other[, col_lst])   
  }
  
  # adpx = adex + adpc (convert to characters)
  adex <- lapply(adex, function(x) as.character(x) ) %>% 
    convert_vars_type(nmdat_data_type)
  adpc <- lapply(adpc, function(x) as.character(x) ) %>% 
    convert_vars_type(nmdat_data_type)
  
  adpx = bind_rows(adex[, col_lst], adpc[, col_lst])  
    
  #--------------------------------------------------------------
  # calculate TIME for adpc
  #--------------------------------------------------------------
  #   adex  %>% .[[c("USUBJID")]] 
  # col_lst = c("USUBJID", "TRTSDTM")
  # adpc = adpc %>% select(-TRTSDTM) %>% 
  #   left_join(adex %>% 
  #               select(one_of(col_lst)) %>% 
  #               distinct(USUBJID, .keep_all=TRUE), 
  #             by="USUBJID")
  # 
  # adpc = adpc %>% mutate(
  #   TIME = difftime(parse_date_time(SAMDTTM, orders="Ymd HMS", truncated = 3), 
  #                   parse_date_time(TRTSDTM, orders="Ymd HMS", truncated = 3), 
  #                   units = "days"
  #   ) %>% as_numeric()
  # )   
  
  #--------------------------------------------------------------
  # bring WGTBL to adex, for calculate EXTDOSE (AMT)
  #--------------------------------------------------------------
  # only need USUBJID and other variables that adex do not have, from adsl
  # col_lst = c("USUBJID", "WGTBL")
  # adex = adex %>% select(-WGTBL)  %>% 
  #   left_join(adsl %>% 
  #               distinct(USUBJID, .keep_all=TRUE) %>% 
  #               select(one_of(col_lst)),
  #             by="USUBJID")
  # 
  # adex = adex %>% mutate(
  #   EXTDOSE = ifelse(EXDOSU=="mg/kg", as_numeric(EXDOSE)*as_numeric(WGTBL),  
  #                    ifelse(EXDOSU=="mg",  as_numeric(EXDOSE), NA)))
  # 

  
  #------------------------
  # TIME 
  #------------------------  
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
  
  adpx = adpx %>% mutate( 
    SAMDTTM = parse_date_time(SAMDTTM, orders=timefmt_var_lst, truncated = 3),  
    EXENDTC = parse_date_time(EXENDTC, orders=timefmt_var_lst, truncated = 3),
    EXSTDTC = parse_date_time(EXSTDTC, orders=timefmt_var_lst, truncated = 3) 
  ) %>% 
    group_by(USUBJID) %>%  mutate(TRTSDTM = min(EXSTDTC, na.rm=TRUE)) %>% 
    ungroup()
  
  ids <- which(is.na(as_numeric(adpx$TIME)))
  adpx$TIME[ids] = difftime(
    adpx$SAMDTTM[ids], adpx$TRTSDTM[ids], units = "days") %>% 
    as_numeric()  
  
  ids <- which(is.na(as_numeric(adpx$TIME)))
  adpx$TIME[ids] = difftime(
    adpx$EXSTDTC[ids], adpx$TRTSDTM[ids], units = "days") %>% 
    as_numeric()  
  
  # EXDUR
  adpx = adpx %>% mutate( 
    EXDUR = difftime(EXENDTC, EXSTDTC, units = "days") %>% as_numeric()
  )
  
  # character
  adpx = adpx %>% 
    mutate(SAMDTTM = as.character(SAMDTTM), 
           TRTSDTM = as.character(TRTSDTM), 
           EXSTDTC = as.character(EXSTDTC), 
           EXENDTC = as.character(EXENDTC)
    )
  
  #------------------
  # merge with adsl
  #------------------
  col_lst = unique(c(setdiff(adsl_var_lst, NULL),   #  "WGTBL"
                     setdiff(colnames(adsl), colnames(adpx))))
  
  # keep "STUDYID", "USUBJID"in adpx, add everything else
  adpx = adpx %>% 
    select(-one_of(setdiff(adsl_var_lst, c("STUDYID", "USUBJID") ))) %>%  
    left_join(adsl%>%distinct(USUBJID,.keep_all=TRUE) %>% 
                select(one_of(col_lst)), 
              by=c("STUDYID", "USUBJID")
    )
  
  #----------------------                  
  # dosing events
  #---------------------- 
  adpx = adpx %>% mutate(
    EXTDOSE = ifelse(EXDOSU=="mg/kg", as_numeric(EXDOSE)*as_numeric(WGTBL),  
                     ifelse(EXDOSU=="mg",  as_numeric(EXDOSE), NA)))
  
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  adpx = adpx[, c(nmdat_var_lst, setdiff(colnames(adpx), nmdat_var_lst))]
  adpx <- adpx %>% convert_vars_type(nmdat_data_type)
  adpx <- adpx %>% dplyr::arrange(STUDYID, USUBJID, TIME, TESTN) 
  adpx <- adpx %>% ungroup()
  
  return(adpx)
}



#################################################################
# final output
#################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adpx <-  build_adpx(dataset$adsl, dataset$adex, dataset$adpc, other=NULL)   
  data[["adpx"]] = adpx
  
  #table <- check_adpx(dataset, adsl, topN=topN)     # dataset: original one, # adsl: parsed one
  output <- list(data=data)
}



