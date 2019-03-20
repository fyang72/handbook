
runSim_by_adpx <-function(mod, 
                          adex, 
                          tgrid = NULL,     # extra timepoint (other than delta)
                          infusion.hour = 1,  # hour,  infusion hours
                          delta = 1,  # integration step
                          followup.period = 84,   # how long of the followup period after treatment
                          seed=1234) {
  
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  # adex
  stopifnot(all(c("POP", "STUDYID", "USUBJID", "ID", "ARMA", "EXROUTE", "WGTBL") %in% colnames(adex)))  # Must have WGTBL
  
  # setup time
  treat_end = adex %>% pull(TIME) %>% as_numeric() %>% max(na.rm=TRUE) # 
  sim_end = treat_end + followup.period   # default 112 days                      # note dose by week only
  if(is.null(tgrid)) {tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7))}
  
  #---------------------------------------- 
  # run simulation
  #---------------------------------------- 
  out = mod  %>% zero_re %>% param(COVARIATE=0, SCALING=0) %>%     # in case user forget
    data_set(adex) %>%   
    mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>%
    as.data.frame() %>% capitalize_names()  %>% slice(2:n())
  
  # add back ID, STUDYID, USUBJID, ARMA, and EXROUTE
  add_col_lst = c("POP", "STUDYID", "USUBJID", "ARMA", "EXROUTE")
  out = out[, setdiff(colnames(out), add_col_lst)]
  out = out  %>% 
    left_join(adex %>% as.data.frame() %>% 
                distinct(ID, .keep_all=TRUE) %>% 
                select(ID, POP, STUDYID, USUBJID, ARMA, EXROUTE),
              by=c("ID"))  
  
  # clean ARMA
  out = out %>% mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE))
  
  # EXSEQ
  out = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% 
    group_by(POP, STUDYID, USUBJID, ARMA) %>%  mutate(EXSEQ=cumsum(EVID))     # for calculat the last dose interval later
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% 
    group_by(POP, STUDYID, USUBJID, ARMA) %>% fill(II) %>% 
    ungroup()
  
  return(out)
}
