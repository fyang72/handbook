
#################################################################
# build_nmdat1, be nmdat-format compatible
#################################################################
#-------------------------------------------------------------  
# adsl: 
#   STUDYID, USUBJID, WGTBL
# adex: 
#   STUDYID, USUBJID, EXENDTC, EXSTDTC, EXDOSE, EXDOSU, EXROUTE, EXTRT, ARMA
# adpc: 
#   STUDYID, USUBJID,	SAMDTTM,	DVOR,	DVORU,	VISIT, 	TIMEPT,	#NTIM,
#   TEST, TESTCD,	TESTCAT, TESTLABL,	BLQ,	LLOQ,	 METHOD
#-------------------------------------------------------------

build_nmdat <- function(dataset) {
  
  nmdat <- dataset
  validate(need(nmdat, message="no nmdat in build_nmdat"))
  
  nmdat = nmdat %>% ungroup() %>% fillUpCol_df(nmdat_var_lst) 
  
  #----------------------------------------
  # call build_adex and build_adpc
  #---------------------------------------- 
  nmdat <- nmdat %>%  
    mutate(EVID = ifelse(!is.na(EXDOSE) & EXDOSE>0, 1, 0))
  
  # adsl 
  adsl = nmdat %>% distinct(USUBJID, .keep_all = TRUE)  %>% 
    build_adsl() %>% ungroup() 
  
  # adex
  adex = nmdat %>% filter(EVID==1) %>% 
    build_adex() %>% ungroup() 
  
  # adpc
  adpc = nmdat %>% filter(EVID==0) %>% 
    build_adpc() %>% ungroup() 
  
  # nmdat
  nmdat = build_adpx(adsl, adex, adpc, other=NULL) %>% as.data.frame()
     
  #------------------------
  # nmdat format specific
  #------------------------
  # nonmem-specific varaibles
  nmdat = nmdat %>% mutate(
    ID   = as.integer(as.factor(USUBJID)),
    DVOR = as_numeric(DVOR), 
    AMT  = as_numeric(EXTDOSE), 
    EVID = ifelse(!is.na(AMT) & AMT>0, 1, 0),
    
    # remove PRE-DOSE, default flag
    CFLAG = ifelse(toupper(ARMA) %in% c("PLACEBO"), "Placebo", 
                   ifelse(TIME<=0 & EVID==0, "Predose",  
                          ifelse(TIME<=0 & EVID==0 & DVOR > 0, "Pre-dose concentration > LLOQ", 
                                 ifelse(is.na(TIME), "Missing Time", 
                                      ifelse(TIME>0 & as.integer(BLQ)==1, "Postdose BLQ", 
                                             as.character(CFLAG)))))),
             
    RATE = as_numeric(AMT)/as_numeric(EXDUR),    
    RATE = ifelse(is.infinite(RATE)|is.na(RATE), 0, RATE), 
    
    CMT = ifelse(EVID==0, 2, 
                 ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), 2,
                        ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), 1, NA)
                        )
                 ),
    
   
    DV = log(DVOR),  
    DV = ifelse(!is.finite(DV), NA, DV),
    MDV = ifelse(is.na(as_numeric(DV)), 1, 0)
    
  )     
   
   
  # EXSEQ 
  nmdat = nmdat %>% group_by(USUBJID) %>% 
    mutate( EXSEQ = cumsum(EVID)
    ) %>% ungroup()
  
  #ROWID 
  if (nrow(nmdat)>0) {nmdat$ROWID=1:nrow(nmdat) }
  nmdat$C[which(!nmdat$CFLAG %in% c(NA,""))] = "C"
  
  #---------------------------------------------
  # order columns, and final output
  #---------------------------------------------     
  nmdat = nmdat[, c(nmdat_var_lst, setdiff(colnames(nmdat), nmdat_var_lst))]
  nmdat <- convert_vars_type(nmdat, nmdat_data_type)
  nmdat <- nmdat %>% dplyr::arrange(STUDYID, USUBJID, TIME, -desc(EVID), TESTN) 
  nmdat <- nmdat %>% ungroup()
  
  return(nmdat)
}


#################################################################
# build_nmdat2, be ready for NONMEM run, add "DROP", colnames not readible
#################################################################

build_nmdat2 <- function(dataset) {
  
  #remove NA, . and empty space
  nmdat <- dataset %>% as.data.frame() %>% ungroup()    # grouped variables cause lots of troubles
  #nmdat[] <- lapply(nmdat, as.character)
  
  # convert all factors to characters
  w <- which( sapply(nmdat, function(x) tail(class(x),1)) == 'factor' )
  nmdat[w] <- lapply(nmdat[w], function(x) as.character(x) )
  
  col_lst = names(which(sapply(nmdat, function(x) tail(class(x),1))=="character"))
  for (i in 1:length(col_lst))  {
    icol = col_lst[i]
    nmdat[, icol] = gsub(" ", "_", nmdat[, icol], fixed=TRUE)
    #nmdat[, icol] = gsub(",", "_", nmdat[, icol], fixed=TRUE)
  }
  nmdat[is.na(nmdat)] = "."
  nmdat = nmdat %>% rename_at(vars(setdiff(col_lst, "C")), ~ paste0(setdiff(col_lst, "C"), "=", "DROP"))
  
  # save the nonmem data  
  #--------------------------------------------------------
  print(paste0(colnames(nmdat), sep=" ", collapse=""))
  
  
  nmdat <- nmdat %>% ungroup()
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
  tabl = nmdat %>% select(USUBJID, ARMA, VISIT, TIMEPT, TIME, SAMDTTM, TRTSDTM ) %>% 
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
  tabl = nmdat %>% select(USUBJID, ARMA, EXTDOSE, EXDOSE, EXDOSU, EVID, WGTBL ) %>% 
    filter(is.na(EXTDOSE), EVID==1)  %>% 
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
  
  nmdat <- build_nmdat(dataset)  
  
  data[["nmdat"]] = nmdat 
  table <- check_nmdat(nmdat, topN=20)   # date_time_format = c("Ymd HMS")
  output <- list(data=data, table=table)
}
