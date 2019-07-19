
# adpx$EVID = as_numeric(adpx$EVID) 
# adex = adpx[which(adpx$EVID!=0), ]; t.adex = paste(adex$USUBJID, adex$TIME, sep="")
# adpc = adpx[which(adpx$EVID==0), ]; t.adpc = paste(adpc$USUBJID, adpc$TIME, sep="")
# adpc = adpc[which(!t.adpc %in% t.adex), ]
# adpx = rbind(adex, adpc)
# adpx = adpx %>% arrange(USUBJID, TIME, -EVID) 
#adpx = adpx[order(adpx$STUDYID, adpx$USUBJID, adpx$TIME, -adpx$EVID), ]  # no ARMA, Since it becomes complicated for multilple doses per subject



#   
# TEST = levels= c("REGN1500", "AngPTL3", "LDL", "." )
# ordered by TESTN
# ARMAN
# 
# EXROUTE MUST BE c("SUBCUTANEOUS", "INTRAVENOUS",
# DUR  EXTDOSE

#parse_date_time   lubridate  /  parse_date_time: User friendly date-time parsing functions





#-----------------------------------------------------------------------------
# convert adpx to NONMEM dataset
#-----------------------------------------------------------------------------

build_nmdat <- function(adpx) {
  
  nmdat = as.data.frame(adpx) 
  
  # fill "." if missing columns
  col.lst = setdiff(adpx.col.lst, colnames(adpx))
  if (length(col.lst)>0) {print( paste0("Warning: ", paste0(col.lst, collapse=", "), " not included in adpx"))}
  nmdat[, col.lst] = "."
  
  
  # "C", "CFLAG", "ROWID", 
  if(!"ROWID" %in% colnames(nmdat)) {nmdat$ROWID = 1:nrow(nmdat)}
  if(!"CFLAG" %in% colnames(nmdat)) {nmdat$CFLAG = "."}
  if(!"C" %in% colnames(nmdat)) {nmdat$C =  "."}
  # 
  # # default BLQ and LLOQ
  # if(!"BLQ" %in% colnames(nmdat)) {nmdat$BLQ = "."}
  # if(!"LLOQ" %in% colnames(nmdat)) {nmdat$LLOQ =  "."}
  # 
  # # default NTIM and TAD
  # if(!"NTIM" %in% colnames(nmdat)) {nmdat$NTIM = "."}
  # if(!"TAD" %in% colnames(nmdat)) {nmdat$TAD =  "."}
  
  # C is a build-in R function, can't be used in ifelse.
  nmdat$C1 = nmdat$C
  
  nmdat = nmdat %>% mutate(
    
    # remove PRE-DOSE, default flag
    CFLAG = ifelse(ARMA %in% c("Placebo"), "Placebo", 
                   ifelse(as_numeric(TIME)<=0, "Pre-Dose",  
                          ifelse(as_numeric(TIME)>0 & as.integer(BLQ)==1, "Post-dose BLQ", as.character(CFLAG)))),
    
    C1 = ifelse(CFLAG %in% c("Placebo", "Pre-Dose", "Post-dose BLQ"), "C", C1), 
      
    ROWID =  as.integer(ROWID), 
    
    ID    = as.integer(as.factor(USUBJID)),
    ARMA  = ordered(ARMA, levels=unique(ARMA)), 
    ARMAN = as.integer(as.factor(ARMA)),
    TIME  = as_numeric(TIME),
    NTIM  = as_numeric(NTIM), 
    TAD   = as_numeric(TAD), 
    
    AMT  = as_numeric(EXTDOSE), 
    RATE = ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), as_numeric(AMT)/as_numeric(DUR),     #    "."            "SUBCUTANEOUS" "INTRAVENOUS" 
                  ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), ".",  ".")),
    RATE = ifelse(is.infinite(RATE)|is.na(RATE), ".", RATE), 
    
    CMT = ifelse(EXROUTE %in% c("IV", "INTRAVENOUS"), 2,
                 ifelse(EXROUTE %in% c("SC", "SUBCUTANEOUS"), 1, 0)),    
    
    EXROUTE = ordered(EXROUTE, levels = c(admin.route.lst, "." )),
    EXROUTN = ifelse(EXROUTE==".", 0, as.integer(as.factor(EXROUTE))),
    
    EVID = ifelse(!is.na(AMT), 1, 0),
    
    TEST = ordered(TEST, levels= unique(TEST)),   #c("REGN1500", "AngPTL3", "LDL", "." )), 
    TESTN = ifelse(TEST==".", 0, as.integer(as.factor(TEST))),
    
    DV = DVOR,  # ifelse(TEST=="REGN1500" & as_numeric(DVOR)!=0, log(as_numeric(DVOR)), DVOR),
    BLQ = ifelse(as_numeric(DVOR)<as_numeric(LLOQ), 1, 0), 
    BLQ = ifelse(is.na(BLQ), 0, BLQ), 
    
    LLOQ = as_numeric(LLOQ), 
    MDV = ifelse(is.na(as_numeric(DV)), 1, 0)
    
  )     
  nmdat = nmdat %>% dplyr::arrange(ID, TIME, desc(EVID))
  nmdat$ROWID = 1:nrow(nmdat)  # reassign ROWID after arrange
  
  # remove C1 (dummy column)
  nmdat$C = nmdat$C1
  nmdat = nmdat %>% select(-C1)
  
  #remove NA, . and empty space
  nmdat = nmdat %>% mutate(SAMDTTM = SAMDTTM %>% as.character())
  nmdat[is.na(nmdat)] = "." 
  colnames(nmdat) <- gsub(".", "_", colnames(nmdat), fixed=TRUE)
  colnames(nmdat) <- gsub(" ", "_", colnames(nmdat), fixed=TRUE)
  
  # remove "," and " " in some columns 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(" ", "_", nmdat[, icol])} 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(",", "_", nmdat[, icol])} 
  
  # add header
  for (i in colnames(nmdat)) {
    nmdat[,i] <- as.character(nmdat[,i]) 
    nmdat[,i] <- gsub(" ", "_", nmdat[,i], fixed=TRUE)
  }
  
  # re-order
  col.lst = c(nmdat.col.lst, setdiff(colnames(nmdat), nmdat.col.lst))
  nmdat = nmdat[, col.lst]
  
  return(nmdat) 
  
}





# remove NA entries
nacols <- function(df) { 
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }

 



