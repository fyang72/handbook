
if (1==2) { 
nmdat <- read_csv("~/handbook/data/nmdat_9SC.csv")
adsl <- read.csv("~/handbook/data/adsl_R2810_SC_9patients_posthoc_params.csv")
adsl <- data.frame(USUBJID="001", WGTBL=75)
adex <- parseARMA("438 mg SC Q3W*1 + 350 mg IV Q3W*3")
cppModel_file <- "~/handbook/model/cpp/LN001_9SC.cpp" 
cppModel=mread(model='cppModel',project=dirname(cppModel_file),file=basename(cppModel_file))

runSim_by_dosing_regimen(
  cppModel,    # model file
  adsl,   # population 
  adex,   # dose regimen
  simulation_delta = 1,  # integration step                  
  tgrid = NULL,     # extra timepoint (other than delta)
  infusion_hrs_lst = 1,  # hour,  infusion hours
  followup_period = 84,   # how long of the followup period after treatment
  seed=1234
) 
  
}

###################################
# runSim_by_dosing_regimen
################################### 
runSim_by_dosing_regimen <- function(
  cppModel,    # model file
  adsl,   # population 
  adex,   # dose regimen
  simulation_delta = 1,  # integration step                  
  tgrid = NULL,     # extra timepoint (other than delta)
  infusion_hrs_lst = 1,  # hour,  infusion hours
  followup_period = 84,   # how long of the followup period after treatment
  seed=1234
  ) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  # Note: 
  # 1) example of adsl = data.frame(USUBJID=1:3, WGTBL=seq(60, 80, by=10)), must have unique USUBJID
  # 
  # 2) example of adex = parseARMA(c("438 mg SC QW*1 + 350 mg IV QW*3", "250 mg SC Q3W*1 + 300 mg IV Q3W*3")) %>% capitalize_names()
  # ARMA                 ID   AMT UNIT  ROUTE FREQ  NDOSE    II  ADDL  EVID  TIME
  # <chr>               <int> <dbl> <chr> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
  # 1 438 mg SC QW*1      1   438 mg    SC    Q1W       1     7     0     1     0
  # 2 350 mg IV QW*3      1   350 mg    IV    Q1W       3     7     2     1     7
  # 3 250 mg SC Q3W*1     2   250 mg    SC    Q3W       1    21     0     1     0
  # 4 300 mg IV Q3W*3     2   300 mg    IV    Q3W       3    21     2     1    21
  # 
  # 3) DOSEID is the dose regimen for a dose group, not actual dose sequence id for a subject.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  adsl = adsl %>% distinct(USUBJID, .keep_all=TRUE)
  
  # adex: expand.grid on DOSEID and ID
  dose = adex %>% capitalize_names() %>% rename(DOSEID = ID)
  adex = expand.grid(DOSEID = unique(dose$DOSEID), USUBJID = unique(adsl$USUBJID)) %>%   #, POP=unique(adsl$POP))
    left_join(dose, by="DOSEID") %>%    # note DOSEID in "dose" could be duplicated
    left_join(adsl, by="USUBJID")  
  
  # ID
  adex = adex %>% 
    mutate(ID = as.integer(as.factor(paste0(USUBJID,"_", DOSEID))))  # New ID , "_", POP
  
  # if multiple dosing regimens (with loading dose)
  adex <- adex %>% 
    left_join(adex %>% group_by(ID) %>% mutate(ARMA2 = paste0(ARMA, collapse="+")) %>% 
                select(ID, ARMA2)%>% distinct(ID, .keep_all=TRUE), 
              by = "ID") %>% 
    dplyr::select(-ARMA) %>% dplyr::rename(ARMA=ARMA2)
  
  # POP + REP + ID + DOSEID(ARMA) + TIME
  adex = adex %>% mutate(AMT = ifelse(UNIT=="mg/kg", AMT * as_numeric(WGTBL), AMT), 
                         
                         CMT = ifelse(ROUTE=="IV", 2, 
                                      ifelse(ROUTE=="SC", 1, 0)), 
                         
                         RATE = ifelse(ROUTE=="IV", AMT/(as_numeric(infusion_hrs_lst[1])/24), 0), 
                         
                         ROUTEN = ifelse(ROUTE=="IV", 1, 
                                         ifelse(ROUTE=="SC", 2, 0))) 
  # treat_end, sim_end
  treat_end1 = ifelse(all(c("ADDL", "II") %in% colnames(adex)), max((adex$ADDL+1)*adex$II), 0) 
  treat_end2 = adex %>% pull(TIME) %>% as_numeric() %>% max(na.rm=TRUE) # 
  treat_end = max(treat_end1, treat_end2)
  
  sim_end = treat_end + followup_period   # default 112 days
  
  # runSim_by_nmdat
  out <- runSim_by_nmdat(
    cppModel = cppModel,   
    adex = adex, 
    simulation_delta = simulation_delta,  # integration step                  
    tgrid = tgrid,     # extra timepoint (other than delta)
    infusion_hrs_lst = infusion_hrs_lst,  # hour,  infusion hours
    treat_end = treat_end, 
    sim_end = sim_end,   # how long of the followup period after treatment
    seed = seed ) 
    
  return(out)
}



runSim_by_nmdat <-function(
  cppModel,   
  adex, 
  simulation_delta = 1,  # integration step                  
  tgrid = NULL,     # extra timepoint (other than delta)
  infusion_hrs_lst = 1,  # hour,  infusion hours
  treat_end = 112, 
  sim_end = 112,   # how long of the followup period after treatment
  seed=1234 ) {
  
  # adex
  stopifnot(all(c("USUBJID", "ID", "ARMA",  "WGTBL") %in% colnames(adex)))  # Must have WGTBL
  
  # setup seed, tgrid
  # -----------------------------------
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  # library("PKPDmisc")
  #sim_end = adex %>% pull(TIME) %>% as_numeric() %>% max(na.rm=TRUE) # 
  
  # note dose by week only
  tgrid = c(tgrid, sim_timept(start=0, 
                              end=treat_end, 
                              delta=simulation_delta, 
                              dtime=seq(0,treat_end, by=7),
                              infusion_hrs_lst =  infusion_hrs_lst #c(5/60, 0.5, 1, 2)
  ))
  tgrid = unique(tgrid) %>% as_numeric()
  tgrid = tgrid[which(!is.na(tgrid))]
  
  # make them as numerics
  # -----------------------------------
  param_lst <- names(param(cppModel)) # intersect(colnames(adex), names(param(cppModel) %>% as.numeric()))
  nonmem_var_lst <- c("ROWID", "ID",  "AMT", "RATE", "CMT", "EVID",
                      "II", "ADDL",  "TIME", "NTIM","VISITNUM",
                      "TESTN", "DV", "DVOR", "BLQ" , "LLOQ", "MDV", 
                      "ARMAN", "EXTRTN", "EXROUTN",  
                      "WGTBL",  "HGTBL",    "BMIBL",  "BSABL",  "AGE", "RACEN", "ETHNICN", 
                      "BLCREAT",  "BLCRCL", "ALTBL", "ASTBL", "BILIBL", 
                      "ALBBL", "IGGBL", "LDHBL","BSABL", "ALPBL"
  )
  var_lst <- unique(c(param_lst, nonmem_var_lst))
  var_lst <- intersect(colnames(adex), var_lst )
  
  adex <- adex %>% mutate_at(var_lst,funs(as_numeric))
  adex = adex %>% filter(TIME>=0)
  #https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
  # dat %>% mutate_if(is.factor, as.character)
  #    
  #   dat %>% mutate_if(is.character, function(x){iconv(x, to = "ASCII//TRANSLIT")})
  # # or to substitute all NA in numeric columns:
  # dat %>% mutate_if(is.numeric, function(x){ifelse(is.na(x), 0, x)})
  # dat %>% mutate_at(vars(matches("fac|fctr|fckr")),funs(factor)) %>%
  # mutate_at(vars(matches("dbl|num|qty")),funs(as.numeric))
  # 
  # run simulation
  # -----------------------------------
  
  # convert all factors to characters
  w <- which( sapply(adex, function(x) tail(class(x),1)) == 'factor' )
  adex[w] <- lapply(adex[w], function(x) as.character(x) )
  col_lst = names(which(sapply(adex, typeof) == "character"))
  
  out = cppModel %>% data_set(adex %>% select(-one_of(col_lst))) %>% 
    mrgsim(end=sim_end, delta=simulation_delta, add=tgrid, tad=TRUE, carry.out=c("II", "EVID")) %>% 
    as.data.frame() %>% capitalize_names()  %>% slice(2:n())
  
  # add back STUDYID, USUBJID, ARMA
  add_col_lst = c("USUBJID", "ARMA")
  out = out[, setdiff(colnames(out), add_col_lst)]
  out = out  %>% 
    left_join(adex %>% as.data.frame() %>% 
                distinct(ID, .keep_all=TRUE) %>% 
                select(one_of(c("ID", add_col_lst))),    
              by=c("ID")
    )  
  
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
  
  return(out)
  
}












