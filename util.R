
# if missing desired variables
# if nmdat is null, return a empty data.frame with column name assigned to nmdat.var.lst
# if not null, return a fillup data.frame with NA in column that not exist before.
fillUpCol_df <- function(nmdat=NULL, nmdat.var.lst="")  {
  col.lst = setdiff(nmdat.var.lst, colnames(nmdat))
  if (length(col.lst)>0) {print(paste0("Warning: missing variable(s) of ", paste0(col.lst, sep=", ", collapse="")))}
  
  if (is.null(nmdat) || nrow(nmdat)==0) {
    nmdat = setNames(data.frame(matrix(ncol = length(nmdat.var.lst), nrow = 0)), nmdat.var.lst)
    colnames(nmdat) = toupper(colnames(nmdat))
    nmdat[] <- lapply(nmdat, as.character)
  }else{
    nmdat[, col.lst] = NA
    colnames(nmdat) = toupper(colnames(nmdat))
  }
  return(nmdat)
}





##   ID time evid   amt cmt   ii addl dose  DENmMOL irep
## 1  1    0    0 0e+00   0    0    0   10 0.000000    1
## 2  1    0    1 1e+07   1 4032    3   10 0.000000    1
## 3  1   12    0 0e+00   0    0    0   10 1.655885    1
## 4  1   34    0 0e+00   0    0    0   10 3.758968    1
## 5  1  168    0 0e+00   0    0    0   10 5.909730    1
## 6  1  336    0 0e+00   0    0    0   10 6.741199    1
sim <- function(n, mod) {
  data <- expand.ev(dose = c(10,60,210), ii = 4032, addl = 3)
  data <- mutate(data, amt = dose*1E6)
  out <- mrgsim(
    mod, data = data,
    add =c(12,34,seq(0,52,1)*168), 
    end = -1, Req = "DENmMOL",
    carry.out = "evid,amt,cmt,ii,addl"
  )
  as_data_frame(out)
}




determineCurrentUser <- function(session=NULL)
{
  if(!is.null(session$user))
  {
    # Extract the username directly, if available.
    username <- tolower(session$user)
  }else if(serverDetails[[AWSnodeName]]$serverType=="Development")
  {
    # If currently being run on the development server, replace the username with the name of the current developer.
    username <- tolower(Sys.info()["user"])
  }else
  {
    # If the username is not directly available and the current server is not the Dev server, the username is unknown.
    username <- "Unknown"
  }
  return(username)
}


######################################################################################
# Run simulation based on adex provided, with or without individual parameters
######################################################################################

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
  treat_end = max((adex$addl+1)*adex$ii)  #l24*7    
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
  
  # nominal time (NTIM)
  #out = out %>% mutate(NTIM = TIME)
  
  # EXSEQ
  out = out %>% #mutate(EVID=ifelse(TAD==0, 1, 0))   %>% 
    group_by(POP, ARMA, USUBJID, ID) %>%  mutate(EXSEQ=cumsum(EVID))     # for calculat the last dose interval later
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% 
    group_by(POP, ARMA, USUBJID, ID) %>% fill(II) %>% 
    ungroup()
  
  return(out)
}





##   ID time evid   amt cmt   ii addl dose  DENmMOL irep
## 1  1    0    0 0e+00   0    0    0   10 0.000000    1
## 2  1    0    1 1e+07   1 4032    3   10 0.000000    1
## 3  1   12    0 0e+00   0    0    0   10 1.655885    1
## 4  1   34    0 0e+00   0    0    0   10 3.758968    1
## 5  1  168    0 0e+00   0    0    0   10 5.909730    1
## 6  1  336    0 0e+00   0    0    0   10 6.741199    1
sim <- function(n, mod) {
  data <- expand.ev(dose = c(10,60,210), ii = 4032, addl = 3)
  data <- mutate(data, amt = dose*1E6)
  out <- mrgsim(
    mod, data = data,
    add =c(12,34,seq(0,52,1)*168), 
    end = -1, Req = "DENmMOL",
    carry.out = "evid,amt,cmt,ii,addl"
  )
  as_data_frame(out)
}



#https://statnmap.com/2017-04-04-format-text-conditionally-with-rmarkdown-chunks-complete-iframed-article/
colFmt = function(x, color){
  outputFormat =  opts_knit$get("rmarkdown.pandoc.to")
  outputFormat = ifelse(is.null(outputFormat), "html", outputFormat) # default html
  
  if(outputFormat == 'latex')
    (paste("\\textcolor{",color,"}{",x,"}",sep=""))
  else if(outputFormat == 'html')
    (paste("<font color='",color,"'>",x,"</font>",sep=""))
  else
    (x)
}





login <- function(login.user.name, login.password) { 
  
  validate(need(login.user.name, message=FALSE)
           #need(login.password, message=FALSE) 
  )
  
  success=FALSE
  #true.password = loginTab %>% filter(user.name==login.user.name) %>% 
  #            pull(password) %>% as.character()
  
  #if (length(true.password)>0 && true.password == login.password) {success=TRUE}
  
  
  if (login.user.name %in% loginTab$user.name) {success=TRUE}
  return(success)
}



VARS<- function(data, vars.lst=c("USUBJID", "ARMA"), Where="indivProfile", Who="feng.yang")  {
  vars.lst = toupper(vars.lst)
  colnames(data) = toupper(colnames(data))
  
  tt=globalVars$magicTab %>% filter(Domain %in% vars.lst, Alias %in% colnames(data),   Who%in%Who) %>%  
    group_by(Domain) %>% arrange(Domain, -Freq) %>% slice(1) %>% as.data.frame()
  rownames(tt) = tt$Domain
  
  alias.lst = tt[as.character(vars.lst), "Alias"]
  return(factor(alias.lst, levels= colnames(data)))
}


push2MagicTab <- function(magicTab, Domain="DVOR", Alias="XYZ", Label="", Where="", Who="feng.yang") {
  
  stopifnot(length(Domain)==length(Alias))
  for (i in 1:length(Domain)) {
    iDomain = Domain[i]
    iAlias = Alias[i]
    tt = magicTab %>% filter(Domain==iDomain, Alias==iAlias,   Who==Who)
    if (nrow(tt)>0) { 
      ids = which(magicTab$Domain==iDomain &  magicTab$Alias==iAlias)
      magicTab[ids, "Freq"] = magicTab[ids, "Freq"] + 1
    }else{
      tt=  data.frame(Domain=iDomain,Alias=iAlias, Freq=1, Label=Label, Where=Where, Who=Who, stringsAsFactors = FALSE)
      magicTab <- bind_rows(magicTab, tt )
    }
  }
  
  return(magicTab)
  
}




# addNewData000000000 <- function(newData, data_name, DATA) {
#   
#   # data itself
#   CurrentLog_mDATA   <- isolate(DATA$mDATA)
#   newData <- list(newData)
#   names(newData) = data_name
#   
#   # meta information for data
#   CurrentLog_mTITLES   <- isolate(DATA$mTITLES)
#   newTitle <- list("DerivedPK")
#   names(newTitle) = data_name
#   
#   if (data_name %in% names(CurrentLog_mDATA)) {
#     cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
#   }
#   DATA$mTITLES[[data_name]] <- newTitle
#   DATA$mDATA[[data_name]] <- newData 
#   
#   return(DATA)
# }


addNewData <- function(newData, data_name, DATA) {
  
  # data itself
  CurrentLog_mDATA   <- isolate(DATA$mDATA)
  
  
  
  if (data_name %in% names(CurrentLog_mDATA)) {
    cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
  }
  
  DATA$mDATA[[data_name]] <- newData 
  
  return(DATA)
}
