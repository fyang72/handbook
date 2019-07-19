
ETAvsCOV <- function(tdata, cov_name_lst=c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL", "BSABL"), 
         eta_name_lst=c("ETA1", "ETA2", "ETA3", "ETA4")) {
  
  library(PKPDmisc)
  library(knitr)
  library(lazyeval)
  library(tidyverse)
  
  #tdata = adpx %>% distinct(USUBJID, .keep_all =TRUE)
  
  # gather all covariates from BW to CRCL to cov_name and cov_value
  #col.lst = c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL",    "BSABL")
  g_eta_cov <- tdata %>%  gather(cov_name, cov_value, one_of(cov_name_lst))
  g_eta_cov$cov_value = as_numeric(g_eta_cov$cov_value)
  
  ## Double stack, We can actually gather again
  #col.lst = c("ETA1", "ETA2", "ETA3", "ETA4")
  g2_eta_cov <- g_eta_cov %>% gather(eta_name, eta_value, one_of(eta_name_lst) )
  g2_eta_cov$eta_value = as_numeric(g2_eta_cov$eta_value)
  
  g2_eta_cov = g2_eta_cov %>% mutate(eta_name =ordered(eta_name, levels=eta_name_lst))  
  g2_eta_cov <- g2_eta_cov %>% filter(!is.na(eta_value), !is.na(cov_value))
  kable(head(g2_eta_cov))
  kable(tail(g2_eta_cov))
  
  
  eta_cov_scatter0 <- function(df, xval = "cov_value", yval, cov_name = "cov_name") {
    lazy_plot <- lazyeval::interp(~ggplot(distinct(df, cov_value, cov_name, ETA1, eta_name), 
                                          aes(x = cov_value, y = ETA1)) +
                                    geom_point() + #+ stat_smooth(se = F) + 
                                    stat_smooth(method = "loess", color = "blue", se = F, size = 1.3) + 
                                    #geom_rug(col=rgb(.5,0,0,alpha=.2)) + 
                                    base_theme() +
                                    ylab("") +   
                                    facet_wrap(~cov_name, scales="free"),
                                  cov_value = as.name(xval),
                                  ETA1 = as.name(yval),
                                  cov_name = as.name(cov_name))
    return(lazyeval::lazy_eval(lazy_plot))
  }
  
  ### plot all releationships 
  split_eta_cov <- g2_eta_cov %>% split(.$cov_name)  
  bp = lapply(split_eta_cov, function(x) {
    cov_name <- unique(x$cov_name)
    eta_cov_scatter0(x, xval = "cov_value", yval="eta_value", cov_name = "cov_name") +  
      facet_wrap(~eta_name, scales = "free") +
      #ggtitle(cov_name) +
      xlab(cov_name) + 
      theme(legend.position = 'none')   
  })
  
  
  return(bp)
}




add_lmFit <- function(fig) { 
  meths <- c("lm","lm","lm","lm" )
  
  # Smoothing function with different behaviour in the different plot panels
  mysmooth <- function(formula,data,...){
    meth <- eval(parse(text=meths[unique(data$PANEL)]))
    x <- match.call()
    x[[1]] <- meth
    eval.parent(x)
  }
  
  formula <- y ~ x 
  
  library(ggpmisc)
  fig = fig + stat_poly_eq(formula = y ~ x,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                           parse=TRUE,label.x.npc = "right") + 
    
    stat_fit_glance(method = 'lm', method.args = list(formula = formula),
                    geom = 'text', aes(label = paste("P-value = ", 
                                                     signif(..p.value.., digits = 4), sep = "")),label.x.npc = 'right',
                    label.y.npc = 0.35, size = 3)   
  return(fig)
}



DVOR.PRED.IPRED <- function(tdata, x="DVOR", y="PRED",  arma ="ARMA", id="ID", legend.nrow=9) { 
  # need ID, DVOR, PRED, IPRED, ARMA
  
  # diagnostic plot
  fig = ggplot(tdata, aes_string(x=x, y= y, col=arma)) + 
    geom_point() +  
    geom_abline(slope=1, lty="dashed") +  
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=legend.nrow,byrow=TRUE))  
  
  # in linear scale
  # --------------------------
  fig_LN = fig  
  fig_LN_Panel = fig_LN + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none") 
  
  
  # In log-scale
  # --------------------------
  fig_LOG = fig
  
  fig_LOG = fig_LOG + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels =   trans_format("log10", math_format(10^.x)))
  
  fig_LOG_Panel = fig_LOG + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none") 
  
  return(list(fig_LN=fig_LN, fig_LN_Panel=fig_LN_Panel,
              fig_LOG=fig_LOG, fig_LOG_Panel=fig_LOG_Panel
  )) 
}


read_runno <- function(MODEL.HOME, subdir="ctl/", runno="LN001", postfix="")  {
  
  #postfix = ".nm7/"
  # need both sdtab001 and patab001,  "IPRED", +  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  library(xpose4) #read in the xopse library 
  
  #xpdb <- xpose.data("001", tab.suffix="", directory=paste(PsN_HOME, subdir, runno, postfix, "\\", sep="")) # run number of xpose table files
  #xpdb <- xpose.data("001", tab.suffix="", directory="H:\\FYANG\\R2810_PD1\\MODEL\\PsN\\LN001\\")
  
  
  directory=paste0(MODEL.HOME, subdir, runno, postfix, "/")
  
  sdtab = read.table(file=paste0(directory, "sdtab001"), skip=1, sep="", header=TRUE)
  patab = read.table(file=paste0(directory, "patab001"), skip=1, sep="", header=TRUE)
  
  lst = read.lst(paste0(directory, runno, ".lst")  )
  
  
  xpdb = left_join(sdtab, 
                   patab[, c("ID", setdiff(colnames(patab), colnames(sdtab)))] %>% distinct(ID, .keep_all=TRUE),
                   by="ID")
  
  
  # xpdb$DV <- exp(as_numeric(xpdb$DV))
  # xpdb$IPRED <- exp(as_numeric(xpdb$IPRED))
  # xpdb$PRED <- exp(as_numeric(xpdb$PRED))
  # 
  
  #-----------------------------------------------------------------------------
  # attach adpx information (from nonmem datafile)
  #----------------------------------------------------------------------------- 
  
  base_ctl = readLines(paste0(directory, runno, ".ctl"),warn=FALSE) 
  
  library("readr")
  library("dplyr")
  
  # need to remove all ; comments
  # need to add ROWID in the dataset (adpx)
  
  tt = unlist(strsplit(base_ctl[which(regexpr('$DATA', base_ctl, fix=TRUE)>0)], " "))     
  tt = gsub("..", "", tt[which(regexpr('.csv', tt, fix=TRUE)>0)], fix=TRUE)
  tt = gsub(",", "", tt, fix=TRUE)
  
  # not use read_csv,  it auto convert double to integer
  adpx <- read.csv(file = paste0(MODEL.HOME, tt), skip=0,  stringsAsFactors=FALSE)
  #adpx <- read_csv(file = paste0(MODEL_HOME, tt), skip=0)
  #adpx %>% filter(ID=="1423390") %>% pull(ALBBL)
  
  
  head(adpx)
  
  return(list(xpdb=xpdb, adpx=adpx, ctl=base_ctl, lst = lst))
  
}




read_PARAMS <- function(MODEL_HOME, runno.lst) {
  # load all modeling result
  PARAMS = lapply(runno.lst, function(runno) {
    library(readr)
    
    tt <- tryCatch(
      {
        #message("This is the 'try' part")
        file.name = paste0(MODEL_HOME, runno, "/raw_results_", runno, ".csv")
        read_csv(file.name)
        #read.lst(paste(MODEL.HOME, "ctl/BST2/", runno, ".NM7\\", runno, ".lst", sep="")  )
      },
      error=function(cond) {
        message(paste("read error:", runno))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        message(paste("readLST caused a warning:", runno))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      finally={
        message(paste0("Successfully read...", runno))
      }
    )     
    
    colnames(tt) = trim(colnames(tt))
    tt
  })
  
  names(PARAMS) = runno.lst
  return(PARAMS)
}




get_summary_params_kable <- function(PARAMS, runno.lst) {
  
  params.name = c("TVCL",       "TVV2" ,      "TVQ",        "TVV3",       "TVVMAX",     "TVKSS" ,     "RUVCV" ,     "RUVSD" ,     "EMAX",       "T50" ,       "HILL"  ,     
                  "WGT_ON_CLQ",  "WGT_ON_VSS", "ALB_ON_CLQ", "IGG_ON_CLQ", "BMI_ON_VSS", "BLK_ON_T50", 
                  "IIV_CLQ",    "IIV_VSS",    "IIV_EMAX" ,  "IIV_T50",    "IIV_VMAX" ,  "IIV_KSS" ,   "OMEGA.2.1.")
  length(params.name)
  params.descp = c("clearance", "central volume \n of distribution", "inter-compartmental \n clearance", "peripheral volume \n of distribution", "maximum rate in \n nonlinear elimination", "Michaelis-Menten \n constant", "proportional error", "additive error", "maximum effect \n in sigmoid model", "half-life to achieve \n half of the maximum effect", "hill exponent \n in Sigmoid model", 
                   "Weight on CL/Q", "Weight on Vss", "Albumin on CL/Q", "IGG on CL/Q", "BMI on Vss", "Black on T50", 
                   "IIV on CL/Q", "IIV of Vss", "IIV of Emax", "IIV on T50", "", "", "IIV between \n CLQ and VSS")
  length(params.descp)
  
  params.unit = c("L/day",       "L" ,      "L/day",        "L",       "L/day",     "mg/L" ,     "" ,     "mg/L" ,     "",       "day" ,       ""  ,     "",  "", "", 
                  "",    "" ,  "",    "" ,  "" ,   "", "" ,  "",    "" ,  "")
  length(params.unit)
  
  meta.param = data.frame(params.name, params.descp, params.unit)
  rownames(meta.param) = params.name
  
  
  tdata= lapply(runno.lst, function(runno) PARAMS[[runno]])
  names(tdata) = runno.lst
  
  out = merge_all(tdata)
  out$model_run_time =  format(out$model_run_time, "%H:%M:%S") #as.character(out$model_run_time)
  
  out$diff_ofv = as_numeric(out$ofv) - as_numeric(out$ofv[1])
  out[, c("ofv", "diff_ofv")] = u.signif(out[, c("ofv", "diff_ofv")], digits=5)
  
  
  #nazeroCols <- function(df) { 
  #  colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN"|x==0)))] }
  #
  #out = out %>% select(-one_of(nazeroCols(out)))
  
  
  col.lst = out %>% select(starts_with("TV"), starts_with("RUV", ignore.case=FALSE), EMAX,T50,HILL, matches("_ON_"), starts_with("IIV", ignore.case=FALSE), OMEGA.2.1.) %>% 
    select(-starts_with("se", ignore.case=FALSE))  %>% colnames()
  
  
  out0 = out %>% select(one_of(col.lst))  
  out1 = out0 %>%  sapply(., as.numeric) #%>% round(., digits=3)
  out2 = out %>% select(one_of(paste0("se", col.lst)))  %>%  sapply(., as.numeric)
  out3 =  abs(out2/out1*100 ) ######????  abs() 01/05/2018
  
  
  txt=gsub("  NA(  NA)", "---", paste0(u.signif(out1, digits=3), "(",   u.signif(out3,digits=3), "%)"), fix=TRUE)
  txt=gsub("(  NA)", "(---)", txt, fix=TRUE)
  
  out3 = matrix(txt, nrow=nrow(out0), dimnames=list(rownames(out0), colnames(out0)))
  
  out3 = out3 %>% as.data.frame()   %>%  select(-TVF1, -TVKA )  %>% t()
  OUT=  cbind(params=rownames(out3), out3)
  rownames(OUT) = 1:nrow(OUT)
  OUT = OUT %>% as.data.frame()
  
  OUT =  cbind(meta.param[OUT%>%pull(params)%>%as.character(), ],  OUT) %>% as.data.frame()
  OUT[, "params.name"] = as.character(OUT[, "params.name"])
  OUT[, "params"] = as.character(OUT[, "params"])
  OUT[, "params.descp"] = as.character(OUT[, "params.descp"])  
  OUT[, "params.unit"] = as.character(OUT[, "params.unit"])    
  
  OUT[which(is.na(OUT$params.name)), "params.name"] = as.character(OUT[which(is.na(OUT$params.name)), "params"])
  OUT = OUT %>% select(params.name, params.descp, params.unit, one_of( runno.lst))
  
  OUT[is.na(OUT)] = ""
  
  return(OUT)
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
  
  adex = adex %>% arrange(ID, DOSEID)
  
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


