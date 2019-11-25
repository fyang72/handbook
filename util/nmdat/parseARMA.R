# https://stackoverflow.com/questions/3028642/regular-expression-for-letters-numbers-and
# ^[a-zA-Z0-9_.-]*$
#   Explanation:
#   
#   ^ is the beginning of the line anchor
# $ is the end of the line anchor
# [...] is a character class definition
# * is "zero-or-more" repetition
#  
# "$THETA  "
# "$OMEGA"
# start <- which(str_detect(base_ctl, "^\\$THETA\\s+"))
# end <- which(str_detect(base_ctl, "^\\$OMEGA\\s+"))
# ids <- intersect(start:end, which(!str_detect(base_ctl, "^\\s*;")))
# 
# pair_lst <- str_extract_all(base_ctl[ids], 
#                "[a-zA-Z0-9_.-]*_ON_[a-zA-Z0-9_.-]*", simplify = FALSE
#                )  %>% unlist() %>% unique()
# 
# 


parseARMA <- function(ARMA) {
  if (1==2)    { # debug}
    ARMA = c(
      "350 mg SC Q3W*5 + 15 mg/kg IV",
      "250 mg SC Q2W + 15 mg/kg IV Q8W",
      "250 mg SC Q2W + 15mg/kg IV Q12W + 450 mg SC Q1W*4", 
      "3mg/kg IV Q2W",
      "3 mg/kg IV QW",
      "3 mg/kg IV Q2W", 
      "3mg/kg Q4W*8 iv"
    )
  }
  
  
  tdata = strVec2matrix(ARMA, sep="\\+")
  
  adex = lapply(tdata, function(i) {i %>% parseARMA0()})  %>% 
    bind_rows()  %>%  arrange(ID) %>% filter(!is.na(amt))
  
  #if (is.null(adex$GROUPID)) { adex$GROUPID= adex$ID }
  #if (is.null(adex$USUBJID)) { adex$USUBJID=paste0(add_prefix(adex$ID, digits=3), "-", 
  #                                                   add_prefix(adex$GROUPID, digits=3)) }
  
  # TIME
  adex = adex %>% mutate(time=ndose*ii) 
  adex = adex %>% group_by(ID) %>% mutate(time=cumsum(time)) %>%   #%>% mutate(time = ifelse(row_number()==1, 0, time)  )
    #mutate_all(funs(lag), n=1)
    mutate(time=lag(time))  %>%   #  %>% na.omit()   #funs(lag), n=1))
    mutate(time=ifelse(is.na(time), 0, time))
  
  # STUDYID, WGTBL, EVID
  #adex = adex %>% mutate( EVID=1)   #STUDYID=1, WGTBL= 75,
  # 
  # # CMT
  # adex = adex %>% mutate(CMT = ifelse(ROUTE %in% c("SC","IM"), 1, 
  #                                     ifelse(ROUTE %in% c("IV"), 2, NA)))  
  # # AMT
  # adex = adex %>% mutate(AMT = ifelse(UNIT %in% c("mg/kg"), AMT*WGTBL, AMT))
  # 
  # # RATE
  # adex = adex %>% mutate(RATE = ifelse(ROUTE %in% c("IV"), AMT/(INFHR/24), 0))
  
  adex
  
  return(adex)
  
  
}



parseARMA0 <- function(ARMA) {
 
    # ARMA = c(
    #   "350 mg SC Q3W*5", # + 15 mg/kg IV",
    #   "3mg/kg IV Q2W",
    #   "3 mg/kg IV Q2W", 
    #   "3mg/kg Q4W*8 iv"
    # )
  ARMA = trim(ARMA)
  ARMA.ORG = ARMA  #unique(ARMA)
  ARMA = toupper(ARMA) #%>% unique()
  
  num_expr <- "([0-9]*\\.?[0-9]+)" 
  
  #amt_expr <- '[0-9]+(\\s+|)(MG|MG/KG)'
  amt_expr <- '([0-9]*\\.?[0-9]+)(\\s+|)(MG|MG/KG)'
  
  AMT = extractExpr(ARMA, regexpr=amt_expr)
  ARMA = removeExpr(ARMA, regexpr=amt_expr)
  amt =   AMT %>% extractExpr(regexpr=num_expr) %>% as.numeric()
  unit = extractExpr(AMT, regexpr='(\\s+|)(MG|MKG|MG/KG)')  %>% trim() %>% tolower()
  unit = ifelse(is.na(unit), "mg", unit)   # Default unit = mg 
  
  route_expr <- '(\\s+|)(IV|SC|IM|IVT)(\\s+|)'
  route = extractExpr(ARMA, regexpr=route_expr)  %>% trim()  
  ARMA = removeExpr(ARMA, regexpr=route_expr)
  route = ifelse(is.na(route), "SC", route)   # Default route=SC 
  
  which_based = ""
  
  # to process QW  (week-based dosing)
  #-----------------------------------------
  freq_ndose_expr <- '(Q)(|[0-9]+)(W)(|([*][0-9]+))'
  freq_ndose = extractExpr(ARMA, regexpr=freq_ndose_expr)  %>% trim()  
  if (!all(is.na(freq_ndose))) {
    which_based = "Week"
    ARMA = removeExpr(ARMA, regexpr=freq_ndose_expr)
    freq_ndose = gsub("QW", "Q1W", freq_ndose, fix=TRUE)
    freq = extractExpr(freq_ndose, regexpr='(Q)[0-9]+(W)')  %>% trim()  
    freq = ifelse(is.na(freq), "Q2W", freq)   # Default freq = QW 
    
    ndose = extractExpr(freq_ndose, regexpr='[*][0-9]+')  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()  
    #ndose = gsub("*","", ndose, fix=TRUE) %>% trim() %>% as.numeric()
    ndose = ifelse(is.na(ndose), 1, ndose)   # Default ndose=1
  }
  
  # to process QD  (day-based dosing)
  #-----------------------------------------
  freq_ndose_expr <- '(Q)(|[0-9]+)(D)(|([*][0-9]+))'
  freq_ndose = extractExpr(ARMA, regexpr=freq_ndose_expr)  %>% trim()  
  if (!all(is.na(freq_ndose))) {
    which_based = "Day"
    ARMA = removeExpr(ARMA, regexpr=freq_ndose_expr)
    freq_ndose = gsub("QD", "Q1D", freq_ndose, fix=TRUE)
    freq = extractExpr(freq_ndose, regexpr='(Q)[0-9]+(D)')  %>% trim()  
    freq = ifelse(is.na(freq), "Q2D", freq)   # Default freq = QW 
    
    ndose = extractExpr(freq_ndose, regexpr='[*][0-9]+')  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()  
    #ndose = gsub("*","", ndose, fix=TRUE) %>% trim() %>% as.numeric()
    ndose = ifelse(is.na(ndose), 1, ndose)   # Default ndose=1
    
  }
  
  
  OUT = bind_cols(ARMA=ARMA.ORG, ID=1:length(amt), amt=amt, unit=unit, route=route, freq=freq, ndose=ndose) #%>% as.data.frame()
 
  # OUT = OUT %>% mutate(ii = extractExpr(freq, regexpr='\\(?[0-9,.]+') %>% as.numeric(), 
  #                      ii = ifelse(is.na(ii), 0,  
  #                              ifelse (which_based=="Week", ii*7,  
  #                                  ifelse (which_based=="Day", ii*1, ii))), 
  #                                 
  #                      addl = ndose-1, 
  #                      evid = 1
  # ) 
  
  # changed 10-05-2018
  OUT = bind_cols(ARMA=ARMA.ORG, ID=1:length(amt), amt=amt, unit=unit, route=route, freq=freq, ndose=ndose) #%>% as.data.frame()
  
  OUT = OUT %>% mutate(ii = extractExpr(freq, regexpr='\\(?[0-9,.]+') %>% as.numeric(), 
                       addl = ndose-1, 
                       evid = 1)
  
  if (which_based=="Week") {
    OUT$ii = OUT$ii * 7
  }else if (which_based == "Day") {
    OUT$ii = OUT$ii * 1
  }
  
  
  
  return(OUT)
}


