
#################################################################
# build_adpx
#################################################################
build_adpx <-function(adsl=NULL, adex=NULL, adpc=NULL, others=NULL) {
 
if (1==2) {   
  source("./script/script4_build_adsl.R")
  source("./script/script4_build_adex.R")
  source("./script/script4_build_adpc.R")
  
  #adsl = read_sas("./data/adsl.sas7bdat")
  #adex = read_sas("./data/adex.sas7bdat")
  #adpc =read_csv("./data/adpc.csv", 
  #                 col_type=cols(.default=col_character()))
   
  adsl=script4_build_adsl(dataset=read_sas("./data/adsl.sas7bdat") )
  adex=script4_build_adex(dataset=read_sas("./data/adex.sas7bdat") %>% 
                            mutate(ARMA=TRTA, 
                                   ARMAN=TRTAN,
                                   EXDOSU = gsub("mL", "mg", EXDOSU, fix=TRUE), 
                                   EXROUTE = ifelse(EXDOSU=="mg/kg", "INTRAVENOUS", 
                                                    ifelse(EXDOSU=="mg", "SUBCUTANEOUS", NA)), 
                                   EXROUTN = ifelse(EXDOSU=="mg/kg", 2, 
                                                    ifelse(EXDOSU=="mg", 1, EXROUTE)) 
                                   )
  )
   
  
  adpc=script4_build_adpc(dataset=read_csv("./data/adpc.csv", 
                                      col_type=cols(.default=col_character())) %>% 
                            mutate(USUBJID = CLID, 
                                   ARMA = paste0(doseplan_lim, " ", DOSU),
                                   PCTPT = TIMEPT, 
                                   DVOR = RESC, 
                                   DVORU = STDUNIT
                                   )
  )
    
}  
  adsl = fillUpCol_df(adsl, adsl.var.lst)
  adex = fillUpCol_df(adex, adpx.var.lst)
  adpc = fillUpCol_df(adpc, adpx.var.lst)
  others = fillUpCol_df(others, adpx.var.lst)
  adpc = bind_rows(adpc, others)
  
  #------------------
  # prepare for adpc
  #------------------ 
  #   adex  %>% .[[c("USUBJID")]] 
  adpc = adpc %>% select(-TRTSDTM) %>% left_join(
    adex  %>% select(USUBJID, TRTSDTM) %>% distinct(USUBJID, .keep_all=TRUE), 
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
  col.lst = c("USUBJID", "WGTBL")
  adex = adex %>% select(-WGTBL)  %>% 
    left_join(adsl %>% distinct(USUBJID, .keep_all=TRUE) %>% 
                select(one_of(col.lst)),
              by="USUBJID")
  
  adex = adex %>% mutate(
    EXTDOSE = ifelse(EXDOSU=="mg/kg", as_numeric(EXDOSE)*as_numeric(WGTBL),  
                     ifelse(EXDOSU=="mg",  as_numeric(EXDOSE), NA)))
  
  
  #------------------
  # merge to adpx
  #------------------
  # time variable must be character
  adpx = bind_rows(adex, adpc)  %>% 
    arrange(STUDYID, ARMA, USUBJID, TIME, TESTCD)
 
  #---------------------
  # from adpx to adpx
  #---------------------
  adpx = adpx %>% mutate(
    
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
    TAD   = as_numeric(TAD), 
    
    AMT  = as_numeric(EXTDOSE), 
    EVID = ifelse(!is.na(AMT)&AMT>0, 1, 0),
    
    RATE = ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), as_numeric(AMT)/as_numeric(EXDUR),     #    "."            "SUBCUTANEOUS" "INTRAVENOUS" 
                  ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), NA,  NA)),
    RATE = ifelse(is.infinite(RATE)|is.na(RATE), NA, RATE), 
    
    CMT = ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), 2,
                 ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), 1, 0)),    
    
    EXROUTE = ordered(EXROUTE, levels = c(admin.route.lst)),
    EXROUTN = ifelse(is.na(EXROUTE), -99, as.integer(as.factor(EXROUTE))),
     
    TESTCD = ordered(TESTCD, levels= unique(TESTCD)),   
    
    TESTCAT = ordered(TESTCAT, levels= testcat.lst),  
    
    DV = DVOR,  # ifelse(TEST=="REGN1500" & as_numeric(DVOR)!=0, log(as_numeric(DVOR)), DVOR),
    BLQ = ifelse(as_numeric(DVOR)<as_numeric(LLOQ), 1, 0), 
    
    LLOQ = as_numeric(LLOQ), 
    MDV = ifelse(is.na(as_numeric(DV)), 1, 0)
    
  )     
  adpx = adpx %>% dplyr::arrange(ID, TIME, desc(EVID)) 
  
  #remove NA, . and empty space
  if (1==2) { 
    adpx[] <- lapply(adpx, as.character)
    
    adpx[is.na(adpx)] = "." 
    colnames(adpx) <- gsub(".", "_", colnames(adpx), fixed=TRUE)
    colnames(adpx) <- gsub(" ", "_", colnames(adpx), fixed=TRUE)
    
    # remove "," and " " in some columns 
    for (icol in 1:ncol(adpx)) {
      adpx[, icol] = gsub(" ", "_", adpx[, icol], fixed=TRUE)
      adpx[, icol] = gsub(",", "_", adpx[, icol], fixed=TRUE)
      } 
  }
  
  #ROWID 
  if ( nrow(adpx)>0) {adpx$ROWID=1:nrow(adpx) }
  adpx$C[which(!adpx$CFLAG %in% c(NA,""))] = "C"
  
  # re-order columns
  adpx = adpx[, c(adpx.var.lst, setdiff(colnames(adpx), adpx.var.lst))]
  
  return(adpx)
}


#################################################################
# check_adex
#################################################################

check_adpx <- function(dataset, topN=20) {
  adpx = dataset %>% ungroup()
  table = NULL
  
  #----------------- 
  # TIME
  #----------------- 
  tabl = adpx %>% select(USUBJID, ARMA, VISIT, PCTPT, TIME, SAMDTTM, TRTSDTM ) %>% 
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
  tabl = adpx %>% select(USUBJID, ARMA, EXTDOSE, EXDOSE, EXDOSU, WGTBL ) %>% 
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
  
  adpx <- build_adpx(dataset, 
                     date_time_format = c("Ymd HMS", "mdY HMS", "bdY HMS"), 
                     dosu.lst = c("mg", "mg/kg"), 
                     admin.route.lst = c("SUBCUTANEOUS", "INTRAVENOUS", "INTRAMUSCULAR", "IVT")
  )    
  
  data[["adpx"]] = adpx 
  table <- check_adpx(adpx, topN=20)   # date_time_format = c("Ymd HMS")
  output <- list(data=data, table=table)
}
