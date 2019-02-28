
#################################################################
# build_adpc
#################################################################

build_adpc <-function(dataset, 
                      date_time_format = c("Ymd HMS", "mdY HMS", "bdY HMS")
                      ) {
   
  # must have these desired variables, if missing, fill with NA 
  adpc = dataset %>% fillUpCol_df(adpc.var.lst) 
  
  #---------------------------------------- 
  # Analysis identifiers
  #---------------------------------------- 
  # STUDYID
  adpc$STUDYID = adpc$STUDYID 
  
  # USUBJID
  if (all(is.na(adpc$USUBJID))) {print("error: all(is.na(USUBJID))=TRUE")}
  
  # standardize USUBJID
  adpc$CLID = adpc$USUBJID 
  study.id = unique(adpc$STUDYID)
  study.id = study.id[which(!is.na(study.id))]
  for (i in 1:max(1,length(study.id))) { 
    adpc$CLID = gsub(study.id[i], "", adpc$CLID, fix=TRUE)
  }
  adpc$CLID = gsub("-", "", adpc$CLID, fix=TRUE)   
  
  t1 = unique(nchar(adpc$CLID))
  t1 = t1[which(!is.na(t1))]
  if (length(t1)==0)  {print("warning: no USUBJID")
  }else if (length(t1)>1)  {print("warning: the length of CLID in adpc are not the same.")
  }else{ 
    if (t1==9) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), substr(CLID, 7, 9), sep="-"))}
    if (t1==6) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), substr(CLID, 4,6), sep="-"))}
    if (t1==3) { adpc = adpc %>% mutate(SUBJECT=paste(substr(CLID, 1, 3), sep="-"))}
    if (!t1 %in% c(3, 6, 9)) {print("nchar(CLID) in adpc !=3, 6 or 9")}
    adpc$USUBJID <- paste(adpc$STUDYID,  adpc$SUBJECT, sep="-") 
    adpc = adpc %>% select(-SUBJECT, -CLID)
  }

   
  
  #----------------------------------------------------------------------------- 
  # Analysis Treatment Variables:   "ARMA" "ARMAN"  
  #----------------------------------------------------------------------------- 
  adpc$ARMA = adpc$ARMA
  
 
  
  #----------------------------------------------------------------------------- 
  # Analysis time variable: "VISIT"  "VISITNUM"  "PCTPT" "NTIM"  "TIME"
  #-----------------------------------------------------------------------------

  adpc$VISIT = toupper(adpc$VISIT)  # paste("Visit ", adpc$VISITNM, sep="") # u.add.prefix(adpc$VISITNM, prefix="", add.number.zero=3), sep="") 
  adpc$VISITNUM = extractExpr(adpc$VISIT, "([0-9]*\\.?[0-9]+)")  %>% as_numeric()
  adpc$VISIT = paste0("VISIT ", adpc$VISITNUM)
  
  if (!all(is.na(adpc$PCTPT)))  {
    adpc$PCTPT = toupper(adpc$PCTPT) 
    if (all(is.na(adpc$NTIM)))  {
      adpc = adpc %>% select(-NTIM) %>% 
        left_join(parsePCTPT(adpc %>% pull(PCTPT) %>% unique()) %>% select(PCTPT, NTIM), by="PCTPT")
    }
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
  
  # c("Ymd HMS",  "db!Y HMS")  "mdY HMS"
  
  if ((!all(class(adpc$SAMDTTM) %in% c("POSIXct", "POSIXt" )))) { 
    library(lubridate) 
    adpc$SAMDTTM = parse_date_time(adpc$SAMDTTM, date_time_format, truncated = 3) %>% as.character()
  } 
    
  #----------------------                  
  # METHOD (SOP), TEST, 
  #---------------------- 
  adpc <- adpc %>%  mutate(METHOD = METHOD, 
                           TEST = TEST,
                           DVOR = DVOR,    # keep those ADA results (which is character-based)
                          
                           DVOR = ifelse(tolower(DVORU)=="ng/ml", as_numeric(DVOR)/1000, DVOR), 
                           DVORU = ifelse(tolower(DVORU)=="ng/ml", "mg/L", DVORU), 
                           
                           DVORU = ifelse(tolower(DVORU)=="ug/ml", "mg/L", DVORU), 
                           
                           LLOQ = ifelse(is.na(as_numeric(LLOQ)), NA, as_numeric(LLOQ)),
                           BLQ = ifelse(as_numeric(DVOR)<LLOQ, 1, 0) 
                         )   
  
 
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  adpc = adpc[, c(adpc.var.lst, setdiff(colnames(adpc), adpc.var.lst))]
  
  return(adpc) 
}



#################################################################
# check_adpc
#################################################################

check_adpc <- function(dataset, adpc, topN=20) {
  adpc = adpc%>% ungroup()
 
  dataset = dataset %>% 
    rename_at(vars(colnames(dataset)),
              ~ paste0(colnames(dataset), "_ORG")
    ) 
  adpc = bind_cols(adpc, dataset)
  
  table = NULL
  #----------------- 
  # ARMA
  #----------------- 
  tabl = adpc %>% select(ARMA, ARMAN, ARMA_ORG) %>% 
    distinct(ARMA, .keep_all=TRUE) %>% 
    arrange(ARMA)
  
  if (nrow(tabl)>topN) {tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of treatment arm (ARMA) and its order (ARMAN)"  
  attr(tabl, "key") = "ARMA_ORG"  
  attr(tabl, "value") = "ARMA"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["ARMA"]] = tabl
  
  #----------------- 
  # PCTPT
  #----------------- 
  tabl = adpc %>% select(PCTPT, PCTPT_ORG, NTIM) %>% distinct(PCTPT, .keep_all=TRUE) %>% 
    mutate(VISITNUM=as_numeric(VISITNUM), 
           NTIM=as_numeric(NTIM)
    ) %>% arrange(VISITNUM, NTIM)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of nominal time point (PCTPT) and its corresponding numerical value (NTIM)" 
  attr(tabl, "key") = "PCTPT_ORG"
  attr(tabl, "value") = "NTIM"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["PCTPT"]] = tabl
  
  #----------------- 
  # SAMDTTM
  #----------------- 
  tabl = adpc %>% select(SAMDTTM, SAMDTTM_ORG, USUBJID, VISIT, PCTPT) %>% 
    filter(is.na(SAMDTTM))  %>% 
    mutate(SAMDTTM = as.character(SAMDTTM),
           SAMDTTM_ORG = as.character(SAMDTTM_ORG)
    ) 
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of sampling time (SAMDTTM) failed to interpret"     
  attr(tabl, "key") = "SAMDTTM_ORG"  
  attr(tabl, "value") = "SAMDTTM"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["SAMDTTM"]] = tabl
  
  
  #----------------- 
  # TESTCD
  #----------------- 
  tabl = adpc %>% select(TESTCD, METHOD, LLOQ, TEST, TESTN, TESTCAT) %>% distinct(TEST, .keep_all=TRUE)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of analyte name (TEST), assay method, LLOQ, its category and its label"     
  attr(tabl, "key") = "TESTCD_ORG"  
  attr(tabl, "value") = "TESTCD"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["TESTCD"]] = tabl
  
  return(table)
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adpc <- build_adpc(dataset, 
                     date_time_format = date_time_format  # global variables. c("Ymd HMS", "mdY HMS", "bdY HMS") 
  )    
  
  data[["adpc"]] = adpc 
  table <- check_adpc(dataset, adpc,  topN=topN)   # date_time_format = c("Ymd HMS")
  output <- list(data=data, table=table)
}
