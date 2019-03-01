
#-------------------------------------
# post-process event to make it adex
#-------------------------------------
process_event <- function(adex) { 
  
  if (1==2) {
    
    event1 = create_event(GROUPID="1:3", TIME=0, AMT="c(100, 200, 300)", UNIT=1, ROUTE=1, INFHR=0, II=14, NDOSE=1)
    event2 = create_event(GROUPID="1:3", TIME=14, AMT="c(10, 20, 30)", UNIT=1, ROUTE=1, INFHR=0, II=14, NDOSE=8)
    adex = rbind(event1, event2) %>% arrange(GROUPID)
    
    adsl = data.frame(ID=1:3, WGTBL=c(60,70,80))
    
    # Example of loading doses
    adex1 = expand.ev(ID=1, time=0, cmt=1, amt=2:5, ii=7, rate=0, addl=0, evid=1) %>% as.data.frame()
    adex2 = expand.ev(ID=1, time=14, cmt=1, amt=seq(20,50,by=10), ii=7, rate=0, addl=8, evid=1) %>% as.data.frame()
    
    adex = rbind(adex1, adex2) #%>% arrange(ID, amt, time)
    adex
    
  }
  
  
  # default,  a SC single dose
  if (is.null(adex$ID))   {adex$ID=1}
  
  if (is.null(adex$time)) {adex$time=0}
  if (is.null(adex$unit)) {adex$unit=1}  # mg
  if (is.null(adex$route)){adex$route=1}  # SC
  if (is.null(adex$infhr)) {adex$infhr=1}  # 1 hour
  if (is.null(adex$ii))   {adex$ii=0} 
  if (is.null(adex$addl)) {adex$addl=0}
  if (is.null(adex$evid)) {adex$evid=1}
  
  if (is.null(adex$cmt))  {adex$cmt=1}   # may derive from route
  if (is.null(adex$rate)) {adex$rate=0}  # may derive from amt/infhr
  
  adex <- adex %>% mutate(time=ifelse(time==0, 0, time), 
                          unit=ifelse(unit==0, mg, unit), 
                          route=ifelse(route==0, SC, route), 
                          
                          infhr=ifelse(is.na(infhr)| (infhr==0), 1, infhr), 
                          
                          ii=ifelse(ii==0, 0, ii), 
                          addl=ifelse(addl==0, 0, addl), 
                          evid=ifelse(evid==0, 1, evid), 
                          cmt=ifelse(cmt==0, 1, cmt), 
                          rate=ifelse(rate==0, 0, rate) 
  )
  
  
  if (is.null(adex$WGTBL)) {adex$WGTBL= 75} 
  if (is.null(adex$STUDYID)) {adex$STUDYID=1}
  
  # filter amt=0
  adex = adex %>% filter(amt!=0, !is.na(amt), amt!="")
  
  # construct ARMA
  adex =adex %>% 
    mutate(
      AMT   = amt, 
      
      UNIT  = ifelse(unit %in% c(mg, "mg"), "mg", 
                     ifelse(unit %in% c(mkg, "mg/kg"), "mg/kg", NA)), 
      ROUTE = ifelse(route %in% c(SC,"SC"), "SC", 
                     ifelse(route %in% c(IV,"IV"), "IV", NA)), 
      NDOSE = as.integer(addl+1), 
      
      FREQ  =  ifelse(ii==0, "", paste0("Q",round(ii/7),"W"))
    )
  
  
  adex = adex %>% mutate(FREQ = gsub("Q1W", "QW", FREQ, fix=TRUE)) 
  adex = adex %>% mutate(ARMA = ifelse(FREQ!="", paste0(AMT, " ", UNIT,  " ", ROUTE, " ", FREQ, "*", NDOSE),
                                       ifelse(FREQ=="", paste0(AMT, " ", UNIT,  " ", ROUTE), NA)  ))
  
  adex = adex %>% group_by(ID) %>% mutate(ARMA=paste0(ARMA, collapse="+"))
  adex = adex %>% data.frame()
  if (is.null(adex$GROUPID)) {adex$GROUPID=as.integer(as.factor(adex$ARMA))}
  if (is.null(adex$USUBJID)) {adex$USUBJID=paste0(add_prefix(adex$ID, digits=3), "-", 
                                                  add_prefix(adex$GROUPID, digits=3))} 
  
  # compartment
  adex = adex %>% mutate(cmt = as.numeric(cmt)) %>% mutate(cmt=ifelse(is.na(cmt), 1, cmt)) 
  adex = adex %>% mutate(cmt = ifelse(route %in% c(SC, "SC"), 1,     # SC
                                      ifelse(route %in% c(IV,"IV"), 2, cmt)) )   # IV
  
  # infhr, amt and rate
  adex = adex %>% mutate(infhr = ifelse(route %in% c(SC, "SC"), 0, infhr), 
                         amt   = ifelse(unit %in% c(mg, "mg"), amt,   # mg
                                        ifelse(unit %in% c(mkg, "mg/kg"), amt*WGTBL, NA)),   # mg/kg
                         rate  = ifelse(route %in% c(SC, "SC"), 0,   # SC
                                        ifelse(route %in% c(IV, "IV"), amt/(infhr/24), NA)))   # IV
  
  # order
  adex = adex %>% arrange(USUBJID, time)
  
  return(adex)
}
