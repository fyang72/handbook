
###################################
# runCurrentSim
###################################
runSim_by_dosing_regimen <-function(
  mod,    # model file
  adsl,   # population 
  dosing.regimen,   # dose regimen
  delta = 1,  # integration step                  
  tgrid = NULL,     # extra timepoint (other than delta)
  infusion.hour = 1,  # hour,  infusion hours
  followup.period = 84,   # how long of the followup period after treatment
  seed=1234) {
  
  # adsl: construct population
  # USUBJID:  is the actual subject ID
  # ID: initially ID=USUBJID, but later expand to expand.grid (ID,DOSEID)
  stopifnot(all(c("POP", "USUBJID", "ID", "WGTBL") %in% colnames(adsl)))  # Must have WGTBL
  adsl$ARMA = NULL   # no ARMA in adsl, will be extracted from dosing.regimen 
  
  # adex: expand.grid on DOSEID and ID
  dose = parseARMA(dosing.regimen) %>% rename(DOSEID = ID)
  adex = expand.grid(DOSEID = unique(dose$DOSEID), ID = unique(adsl$ID)) %>%   #, POP=unique(adsl$POP))
    mutate(DOSEID = as.integer(DOSEID), 
           ID = as.integer(ID)) %>% 
    left_join(dose, by="DOSEID") %>% 
    left_join(adsl %>% mutate(ID=as.integer(ID)), by="ID")
  
  # POP + REP + ID + DOSEID(ARMA) + TIME
  adex = adex %>% mutate(ID = as.integer(as.factor(paste0(ID,"_", DOSEID, "_", POP))),  # New ID
                         amt = ifelse(unit=="mg/kg", amt * as_numeric(WGTBL), amt), 
                         cmt = ifelse(route=="IV", 2, 
                                      ifelse(route=="SC", 1, 0)), 
                         rate = ifelse(route=="IV", amt/(infusion.hour/24), 0), 
                         
                         routen = ifelse(route=="IV", 1, 
                                         ifelse(route=="SC", 2, 0))) 
  
  # construct ARMA
  adex = adex %>%  
    group_by(ID) %>%
    mutate(ARMA = paste0(unique(ARMA), collapse=" + ")) %>% ungroup() %>%  
    mutate(ARMA = ordered(ARMA, levels = unique(ARMA)))   
  
  adex$ARMA %>% unique()
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  # library("PKPDmisc")
  treat_end = adex %>% group_by(ID) %>% 
    dplyr::summarise(sim_end = sum((addl+1)*ii)) %>% ungroup() %>% 
    pull(sim_end) %>% max()   
  sim_end = treat_end + followup.period   # default 112 days                      # note dose by week only
  if(is.null(tgrid)) {tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7))}
  
  #---------------------------------------- 
  # run simulation
  #---------------------------------------- 
  out = mod %>% data_set(adex) %>% 
    mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid", "routen", "amt", "cmt", "rate")) %>% 
    as.data.frame() %>% capitalize_names()  %>% slice(2:n())
  
  
  # add back ID, STUDYID, USUBJID, ARMA
  out = out %>% left_join(adex %>% as.data.frame() %>% 
                            distinct(ID, .keep_all=TRUE) %>% 
                            select(ID, POP, ARMA, USUBJID),
                          by=c("ID")) %>% 
    mutate(ARMA = ordered(ARMA, levels = unique(ARMA)))

  
  # EXSEQ
  out = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% 
    group_by(POP, ARMA, USUBJID, ID) %>%  mutate(EXSEQ=cumsum(EVID))     # for calculat the last dose interval later
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% 
    group_by(POP, ARMA, USUBJID, ID) %>% fill(II) %>% 
    ungroup()
  
  return(out)
}



# use in "handbook", adex is outside

runSim_by_dosing_regimen2 <-function(
  cppModel,    # model file
  adsl,   # population 
  adex,   # dose regimen
  simulation_delta = 1,  # integration step                  
  tgrid = NULL,     # extra timepoint (other than delta)
  infusion_hrs = 1,  # hour,  infusion hours
  followup_period = 84,   # how long of the followup period after treatment
  seed=1234) {
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # adex: expand.grid on DOSEID and ID
  dose = adex %>% capitalize_names() %>% rename(DOSEID = ID)
  adex = expand.grid(DOSEID = unique(dose$DOSEID), USUBJID = unique(adsl$USUBJID)) %>%   #, POP=unique(adsl$POP))
    left_join(dose, by="DOSEID") %>% 
    left_join(adsl, by="USUBJID") %>% 
    mutate(WGTBL = as_numeric(WGTBL))  
  
  adex = adex %>% 
    mutate(ID = as.integer(as.factor(paste0(USUBJID,"_", DOSEID))))  # New ID , "_", POP
  
  # POP + REP + ID + DOSEID(ARMA) + TIME
  adex = adex %>% mutate(AMT = ifelse(UNIT=="mg/kg", AMT * as_numeric(WGTBL), AMT), 
                         CMT = ifelse(ROUTE=="IV", 2, 
                                      ifelse(ROUTE=="SC", 1, 0)), 
                         RATE = ifelse(ROUTE=="IV", AMT/(infusion_hrs/24), 0), 
                         
                         ROUTEN = ifelse(ROUTE=="IV", 1, 
                                         ifelse(ROUTE=="SC", 2, 0))) 
  
  
  # setup parameters for simulation
  # -----------------------------------
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  # library("PKPDmisc")
  treat_end1 = ifelse(all(c("ADDL", "II") %in% colnames(adex)), max((adex$ADDL+1)*adex$II), 0) 
  treat_end2 = adex %>% pull(TIME) %>% as_numeric() %>% max(na.rm=TRUE) # 
  treat_end = max(treat_end1, treat_end2)
  
  sim_end = treat_end + followup_period   # default 112 days  
  
  
  # note dose by week only
  tgrid = sim_timept(start=0, 
                     end=treat_end, 
                     delta=simulation_delta, 
                     dtime=seq(0,treat_end, by=7))
  
  # run simulation
  # -----------------------------------
  col_lst = names(which(sapply(adex, typeof) == "character"))
  
  out = cppModel %>% data_set(adex %>% select(-one_of(col_lst))) %>% 
    mrgsim(end=sim_end, delta=simulation_delta, add=tgrid, tad=TRUE, carry.out=c("II", "EVID")) %>% 
    as.data.frame() %>% capitalize_names()  %>% slice(2:n())
  
  # add back ID, STUDYID, USUBJID, ARMA, and EXROUTE
  add_col_lst = c("ARMA", "ROUTE", "WGTBL")
  out = out[, setdiff(colnames(out), add_col_lst)]
  out = out  %>% 
    left_join(adex %>% as.data.frame() %>% 
                distinct(ID, .keep_all=TRUE) %>% 
                select(ID, USUBJID, ARMA, WGTBL),
              by=c("ID"))  
  
  col.lst <- colnames(out)[which(substr(colnames(out), 1,6) == "IPRED_")]
  out = out %>% gather(TEST, IPRED, -one_of(setdiff(colnames(out), col.lst))) %>% 
    mutate(TEST=gsub("IPRED_", "", TEST, fixed=TRUE))
  
  # col.lst <- colnames(out)[which(substr(colnames(out), 1,3) == "DV_")]
  # out = out %>% gather(TEST, DV, -one_of(setdiff(colnames(out), col.lst)))
  # 
  
  # clean ARMA
  #out = out %>% mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE))
  
  # EXSEQ
  out = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% 
    group_by(ID) %>%  mutate(EXSEQ=cumsum(EVID))     # for calculat the last dose interval later
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% 
    group_by(ID) %>% fill(II) %>% 
    ungroup()
  
  head(out) %>% as.data.frame()
  
  return(out)
}

