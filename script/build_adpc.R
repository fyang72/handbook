
########################################################################
# build_adpc
########################################################################

build_adpc <-function(dataset ) {
  
  # must have these desired variables, if missing, fill with NA 
  adpc = dataset %>% fillUpCol_df(adpc_var_lst) 
  
  #--------------------------------------------------------------
  # Analysis identifiers
  #--------------------------------------------------------------
  # STUDYID
  adpc <- adpc %>% mutate(STUDYID = STUDYID %>% as.character())
  
  # USUBJID
  if (all(is.na(adpc$USUBJID))) {
    print("error: all(is.na(USUBJID))=TRUE")}
  
  adpc <- adpc %>% mutate(
    USUBJID = standardise_USUBJID(STUDYID, USUBJID) %>% as.character()
  )
 
  #--------------------------------------------------------------
  # Analysis Treatment Variables:   "ARMA" "ARMAN"  
  #--------------------------------------------------------------
  adpc <- adpc %>% mutate(
    ARMA = ordered(ARMA, levels=unique(ARMA)), 
    ARMAN = as.integer(ARMA), 
    ARMA = as.character(ARMA)
  )
 
  #--------------------------------------------------------------
  # Analysis time variable: "VISIT"  "VISITNUM"  "TIMEPT" "NTIM"  "TIME"
  #--------------------------------------------------------------
  adpc <- adpc %>% mutate(
    VISIT = toupper(as.character(VISIT)),    
    VISITNUM = extractExpr(VISIT, "([0-9]+)") %>% as.integer(), 
    VISIT = paste0("VISIT ", VISITNUM)
  )
  
  # TIMEPT and NTIM
  adpc <- adpc %>% mutate(TIMEPT = toupper(adpc$TIMEPT))
  if (all(is.na(adpc$NTIM)))  {
    adpc = adpc %>% select(-NTIM) %>% mutate(TIMEPT=as.character(TIMEPT)) %>% 
      left_join(
        parseTIMEPT(adpc %>% pull(TIMEPT) %>% unique()) %>% 
          select(TIMEPT, NTIM) %>% mutate(TIMEPT=as.character(TIMEPT)) , 
        by="TIMEPT")
  }
  
  #--------------------------------------------------------------  
  # TIME
  #-------------------------------------------------------------- 
  adpc = adpc %>% mutate( 
    SAMDTTM = as.character(SAMDTTM) 
  ) 
   
    
  #--------------------------------------------------------------                 
  # METHOD (SOP), TEST, 
  #--------------------------------------------------------------
  adpc <- adpc %>%  mutate(
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
  
  #--------------------------------------------------------------
  # combine dataset and adpc
  #--------------------------------------------------------------   
  dataset = dataset %>% 
    rename_at(vars(colnames(dataset)),
              ~ paste0(colnames(dataset), "_ORG")
    ) 
  
  # assume adpc hasn't been filtered or arranged
  adpc = bind_cols(adpc, dataset)
  
  #--------------------------------------------------------------
  # order columns, and final output
  #--------------------------------------------------------------  
  adpc = adpc[, c(adpc_var_lst, setdiff(colnames(adpc), adpc_var_lst))]
  adpc <- convert_vars_type(adpc, adpc_data_type)
  adpc <- adpc %>% dplyr::arrange(STUDYID, USUBJID, TIME, TESTN) # DO NOT RE-ARRANGE
  adpc <- adpc %>% ungroup()
  
  return(adpc) 
}



########################################################################
# check_adpc
########################################################################

check_adpc <- function(adpc, topN=20) {
  adpc = adpc %>% ungroup()
 
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
  # TIMEPT
  #----------------- 
  tabl = adpc %>% select(VISITNUM, TIMEPT, TIMEPT_ORG, NTIM) %>% distinct(TIMEPT, .keep_all=TRUE) %>% 
    mutate(VISITNUM=as_numeric(VISITNUM), 
           NTIM=as_numeric(NTIM)
    ) %>% arrange(VISITNUM, NTIM) %>% 
    select(-VISITNUM)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of nominal time point (TIMEPT) and its corresponding numerical value (NTIM)" 
  attr(tabl, "key") = "TIMEPT_ORG"
  attr(tabl, "value") = "NTIM"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["TIMEPT"]] = tabl
  
  #----------------- 
  # SAMDTTM
  #----------------- 
  tabl = adpc %>% select(SAMDTTM, SAMDTTM_ORG, USUBJID, VISIT, TIMEPT) %>% 
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
  tabl = adpc %>% select(TESTCD, TESTCD_ORG, METHOD, LLOQ, TEST, TESTN, TESTCAT) %>% distinct(TEST, .keep_all=TRUE)
  
  if (nrow(tabl)>topN) { tabl = tabl %>% slice(1:topN) }
  attr(tabl, "title") = "List of analyte name (TEST), assay method, LLOQ, its category and its label"     
  attr(tabl, "key") = "TESTCD_ORG"  
  attr(tabl, "value") = "TESTCD"
  if (nrow(tabl)>topN) {attr(tabl, "footnote") = paste0("Note, the default is to display top ", topN, " rows.")}
  table[["TESTCD"]] = tabl
  
  return(table)
}

########################################################################
# final output
########################################################################
if (ihandbook) {
  data = NULL
  table = NULL
  
  adpc <- build_adpc(dataset
           #timefmt_var_lst = timefmt_var_lst  # global variables. c("Ymd HMS", "mdY HMS", "bdY HMS") 
  )    
  
  data[["adpc"]] = adpc 
  table <- check_adpc(adpc, topN=topN)   # timefmt_var_lst = c("Ymd HMS")
  output <- list(data=data, table=table)
}
