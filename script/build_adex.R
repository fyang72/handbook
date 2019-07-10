
########################################################################
# build_adex
########################################################################
build_adex <-function(dataset) {
  
  # must have these desired variables, if missing, fill with NA 
  adex = dataset %>% fillUpCol_df(adex_var_lst) 
    
  #--------------------------------------------------------------
  # Analysis identifiers
  #--------------------------------------------------------------
  # STUDYID
  adex <- adex %>% mutate(STUDYID = STUDYID %>% as.character())
  
  # USUBJID
  if (all(is.na(adex$USUBJID))) {
    print("error: all(is.na(USUBJID))=TRUE")}
  
  adex <- adex %>% mutate(
    USUBJID = standardise_USUBJID(STUDYID, USUBJID) %>% 
      as.character()
  )
  
  
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
   
  adex <- adex %>% mutate(
    ARMA = ordered(ARMA, levels=unique(ARMA)), 
    ARMAN = as.integer(ARMA), 
    ARMA = as.character(ARMA)
  )
  
  #----------------------------------------------------------------------------- 
  # VISIT LEVEL:   VISIT VISITNUM
  #----------------------------------------------------------------------------- 
  adex <- adex %>% mutate(
    VISIT = toupper(as.character(VISIT)),    
    VISITNUM = extractExpr(VISIT, "([0-9]+)") %>% as.integer(), 
    VISIT = paste0("VISIT ", VISITNUM)
  )
   
  #----------------------------------------------------------------------------- 
  #  EXTRT EXTRTN
  #----------------------------------------------------------------------------- 
  adex <- adex %>% mutate(
    EXTRT = ordered(EXTRT, levels=unique(EXTRT)), 
    EXTRTN = as.integer(EXTRT), 
    EXTRT = as.character(EXTRT)
  ) 
  
  #---------------------------------------------
  # dosing variable: "EXSTDTC",  "EXENDTC", "EXDUR", "TRTSDTM", "TIME"
  #--------------------------------------------- 
   
  library(lubridate) 
  adex = adex %>% mutate( 
    EXENDTC = as.character(EXENDTC),
    EXSTDTC = as.character(EXSTDTC)
  )  
 
  
  #---------------------------------------------
  # "EXROUTE"    "SUBCUTANEOUS"  "INTRAVENOUS"   "INTRAMUSCULAR" "IVT"      
  #--------------------------------------------- 
  adex = adex %>% mutate(
    EXROUTE = toupper(EXROUTE), 
    EXROUTE = str_replace(EXROUTE, "^IV$", "INTRAVENOUS"),     
    EXROUTE = str_replace(EXROUTE, "^SC$", "SUBCUTANEOUS"), 
    EXROUTE = str_replace(EXROUTE, "^IM$", "INTRAMUSCULAR"), 
    EXROUTE = ordered(toupper(EXROUTE), levels = route_var_lst),  
    EXROUTN = ifelse(is.na(EXROUTE), -99, as.integer(EXROUTE)), 
    EXROUTE = as.character(EXROUTE),
    
    EXDOSE = as_numeric(EXDOSE), 
    EXDOSU = ordered(EXDOSU, levels = dosu_var_lst),  
    EXDOSU = as.character(EXDOSU)
  )
  
  #---------------------------------------------
  # EXSEQ
  #--------------------------------------------- 
  adex = adex %>% group_by(USUBJID) %>% 
    mutate(EVID = ifelse(as_numeric(EXDOSE)>0, 1, 0), 
           EXSEQ = cumsum(EVID)
           ) %>%  
    ungroup()
   
  #--------------------------------------------------------------
  # combine dataset and adex
  #-------------------------------------------------------------- 
  dataset = dataset %>% ungroup() %>% 
    rename_at(
      vars(colnames(dataset)), ~ paste0(colnames(dataset), "_ORG")
    ) 
  
  # assume adex hasn't been filtered or arranged
  adex = bind_cols(adex, dataset)
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------   
  adex <- adex[, c(adex_var_lst, setdiff(colnames(adex), adex_var_lst))]  
  adex <- convert_vars_type(adex, adex_data_type)
  adex <- adex %>% dplyr::arrange(STUDYID, USUBJID, TIME)  
  adex <- adex %>% ungroup()
  
  return(adex) 
}


########################################################################
# check_adex
########################################################################

check_adex <- function(adex, topN=20) { 
  adex <- adex %>% ungroup()
  
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
  
  adex <- build_adex(dataset#, 
                    #timefmt_var_lst = timefmt_var_lst, 
                    #dosu_var_lst = dosu_var_lst, 
                    #route_var_lst = route_var_lst
                    )    
  
  data[["adex"]] = adex 
  table <- check_adex(adex, topN=topN)    
  output <- list(data=data, table=table)
}
