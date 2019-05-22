

    
#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  #-----------------------------------------------------------------------------
  # setup default integral time points based on t.last, t.infinity and time.interval
  #-----------------------------------------------------------------------------  
  run_Rsim <- function(lst, adsl.lst, ADEX, mod1, treat_end, sim_end) {
 
    #adsl.lst = adsl.lst[lst$POP]
    
    tt = lapply(1:nrow(lst), function(i, lst, adsl.lst, ADEX, mod1) {
    
        print(i) 
        adsl = adsl.lst[[as.character(lst[i, "POP"])]]
        adex = create_adex(adsl, ADEX) 
         
        #treat_end = 24*7;  sim_end = 32*7
        tgrid = sim_timept(start=0, end=sim_end, delta=1, dtime=seq(0,treat_end, by=7))
      
        out = mod1 %>% param(TVF1=as_numeric(lst[i, "F1"])) %>%  
               #omat(CL_V2_Q=dmat(0,0,0), IIV_ETA=dmat(0), IIV_EV3=dmat(0), IIV_EVMAX=dmat(0), IIV_EKSS=dmat(0)) %>%  
                ev(adex) %>% mrgsim(end=sim_end, delta=1, add=tgrid) %>% as.data.frame() %>% capitalize_names()
                    
        out = out %>% left_join(adex %>% as.data.frame() %>% select(ID, POP, STUDYID, USUBJID, ARMA), by="ID") %>% mutate(ARMA = trim(gsub("INFUSION", "", ARMA, fix=TRUE)))
  
       }, lst, adsl.lst, ADEX, mod1)
        
       
    RESULT = tt %>% bind_rows()  
    return(RESULT)
  }

     
     
#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)     
  # update model parameters  
  param2 <- function(mod, PARAM) {
    PARAM = paste(paste(names(PARAM), "=", PARAM, sep=""), collapse=", ")
    eval(parse(text=paste("mod0 <- mod %>% param(",PARAM,")",sep=""))) }
    
    
     

################################################################################  
################################################################################
#  a list of function to batch process simulation
################################################################################
################################################################################

  #----------------------------------------------------------------------------- 
  # create_adsl
  #-----------------------------------------------------------------------------  
  
  # based on sampled WGTBL, 
  create_adsl = function(POP="", nsubject, meanWt=75, sdWt=0.2*meanWt, lowerWt=meanWt*0.5, upperWt=meanWt*2, seed =1234) { 
      nsample = round(nsubject*1.5, digits = 0)  
      
      adsl = NULL
      adsl$ID = seq(1, nsample, by=1)
      adsl$USUBJID =  add_prefix(adsl$ID, prefix="", digits=4)
      adsl$POP = POP
      
      set.seed(seed); 
      adsl$WGTBL <- rnorm(nsample, meanWt, sdWt); 
        
      adsl = as.data.frame(adsl)        
      adsl = adsl %>% dplyr::filter(WGTBL>=lowerWt & WGTBL<=upperWt);   
      adsl = adsl[1:nsubject, ] ; 
            

      adsl[, c("AGE", "HGTBL", "BMIBL", "BSABL","SEX", "SEXN", "RACE", "RACEN", "ETHNIC", "ETHNICN")] = 0
      
      stopifnot(!is.na(adsl$WGTBL))
      
      return(adsl)
      }
       
  #----------------------------------------------------------------------------- 
  # create_adex0
  #-----------------------------------------------------------------------------  
  
  # Q3W*8, 300, 450, 600 mg SC  # bind_rows(
  create_adex0 <- function(adsl, STUDYID, GROUP, START_DAY,  DOSE_LEVEL, DOSE_UNIT, NUM_DOSE,  ROUTE, INFUSION_HOUR=1, DOSE_FREQ, ARMA) { 

    IV.NAMES = c("IV", "IV BOLUS", "IV INFUSION")
    INFUSION_HOUR = as_numeric(INFUSION_HOUR)
    adsl$SUBJID = adsl$USUBJID
    
    CMT = ifelse(ROUTE %in% IV.NAMES, 2, 1)
 
    adex <- expand.ev(
      amt=DOSE_LEVEL, cmt=CMT, ii=DOSE_FREQ, evid=1, addl=NUM_DOSE-1, time=START_DAY, 
      STUDYID=STUDYID, GROUP=GROUP, ROUTE=ROUTE, DOSE_UNIT=DOSE_UNIT, INFUSION_HOUR=INFUSION_HOUR, ARMA=ARMA)  #%>% arrange(time) %>% as.data.frame %>% as.ev  
    adex$USUBJID =  add_prefix(adex$ID, prefix="", digits=3)
    adex$DOSEID = adex$USUBJID
    
    adex$ARMA = as.character(adex$ARMA)
    adex$ARMA = ifelse(adex$ARMA=="", create_ARMA(DOSE_LEVEL, DOSE_UNIT, DOSE_FREQ, NUM_DOSE, ROUTE), adex$ARMA)
    #adex = adex %>%  capitalize_names()
    
    tt = expand.grid(SUBJID=adsl$USUBJID, DOSEID=adex$USUBJID) %>% mutate(DOSEID=as.character(DOSEID))
    tt = tt %>% left_join(select(adex, -USUBJID, -ID), by= "DOSEID")
    
    adsl = adsl[, c("SUBJID", setdiff(colnames(adsl), colnames(tt)))]
    adsl = adsl[, !colnames(adsl) %in% c("TIME", "CMT", "AMT", "EVID", "RATE")]
    tt = tt %>% left_join(adsl, by= "SUBJID")
    tt = tt %>% mutate(amt = ifelse(DOSE_UNIT=="mg/kg", amt*WGTBL, amt), 
                       rate = ifelse(INFUSION_HOUR==0, 0, amt/(INFUSION_HOUR/24)))
        

    
    return(tt)
    }
     
  #----------------------------------------------------------------------------- 
  # create_adex
  #-----------------------------------------------------------------------------  
  
  create_adex <- function(adsl, ADEX) { 
    
    ADEX = data.frame(ADEX)
     
    tt = NULL
    for (i in 1:nrow(ADEX)) {
          STUDYID = ADEX[i, "STUDYID"]
          GROUP = ADEX[i, "GROUP"]
          START_DAY = ADEX[i, "START_DAY"]
          DOSE_LEVEL = eval(parse(text=paste(
          "c(", ADEX[i, "DOSE_LEVEL"],")", sep="") ))        #ADEX[i, "DOSE_LEVEL"]
          DOSE_UNIT = ADEX[i, "DOSE_UNIT"]
          NUM_DOSE = ADEX[i, "NUM_DOSE"]
          ROUTE = ADEX[i, "ROUTE"]
          INFUSION_HOUR = ADEX[i, "INFUSION_HOUR"]
          DOSE_FREQ = ADEX[i, "DOSE_FREQ"]
          ARMA = ADEX[i, "ARMA"]
          
          tt=rbind(tt, create_adex0(adsl, STUDYID, GROUP, START_DAY,  DOSE_LEVEL, DOSE_UNIT, NUM_DOSE,  ROUTE, INFUSION_HOUR, DOSE_FREQ, ARMA))
      }
      
    tt$STUDYID = add_prefix(tt$STUDYID, prefix="", digits=3)
    tt$GROUP = add_prefix(tt$GROUP, prefix="", digits=3)
    tt$USUBJID = paste(tt$POP, tt$SUBJID, tt$STUDYID, tt$GROUP, tt$DOSEID, sep="-")
    tt$ID = as.integer(as.factor(tt$USUBJID))
    
    lst1 = c("USUBJID", "ID", "POP","SUBJID", "STUDYID", "GROUP",  "DOSEID","ARMA")
    lst2 = setdiff(colnames(tt), lst1)
    lst = setdiff(c(lst1, lst2), colnames(tt))
    # print(lst)
     
    tt = tt[, c(lst1, lst2)]
    
    tt = tt %>% arrange(USUBJID, time) %>% as.data.frame()  #  %>% as.ev()  
     
    return(tt)
    }      
    


  #----------------------------------------------------------------------------- 
  # create_ARMA
  #-----------------------------------------------------------------------------  
  
  create_ARMA <- function(DOSE_LEVEL, DOSE_UNIT, DOSE_FREQ, NUM_DOSE, ROUTE) {
    
    # http://www.cwladis.com/math104/lecture5.php   
    #  subcultaneously (subQ), intramuscularly (IM), or intravenously (IV).
    #  q.h.     q.2.h.     q.3.h.    q.4.h.
    #  q.d.    (q.o.d.)
    #  b.i.d   twice a day
    #  t.i.d   three times a day
    #  q.i.d   four times a day 
    EXDINT = NULL
    ids = which(round(DOSE_FREQ/7,digits=0)>0);   {EXDINT[ids]  = paste("Q",round(DOSE_FREQ[ids]/7,digits=0),"W",sep="") }    # Dose Interval, in weeks 
    ids = which(round(DOSE_FREQ/7,digits=0)==0);  {EXDINT[ids]  = paste("Q",round(DOSE_FREQ[ids],digits=0),"D",sep="") }    # Dose Interval, in weeks 
    ids = which(DOSE_FREQ==1/2);  EXDINT[ids]  = "BID"     # Dose Interval, in weeks 
    ids = which(DOSE_FREQ==1/3);  EXDINT[ids]  = "TID"     # Dose Interval, in weeks 
    ids = which(DOSE_FREQ==1/4);  EXDINT[ids]  = "QID"     # Dose Interval, in weeks 
    ids = which(DOSE_FREQ==7/2);  EXDINT[ids]  = "BIW"     # Dose Interval, in weeks 
   
    # EXDOSE LEVEL:   ARMA
    ARMA    = paste(DOSE_LEVEL, " ", DOSE_UNIT, " ",  EXDINT, "*", NUM_DOSE, " ", ROUTE, sep="")
    return(ARMA)
  }
  

  #----------------------------------------------------------------------------- 
  # calc_RTAB
  #-----------------------------------------------------------------------------  
  
  calc_RTAB <- function(tdata, POP="POP", F1="F1",ARMA="ARMA",USUBJID="USUBJID",TIME="TIME", DVOR="DVOR", T.END=24*7, AUC12wk = 12) {
  #  T.END : treatment period ends
  #
      tdata = as.data.frame(tdata)

      tdata$POP = tdata[, POP]
      tdata$ARMA = tdata[, ARMA]      
      tdata$USUBJID = tdata[, USUBJID]
      tdata$TIME = as_numeric(tdata[, TIME])
      tdata$DVOR = as_numeric(tdata[, DVOR])      
      tdata$F1 = as_numeric(tdata[, F1])

      #T.END = 24*7   #  24 weeks treatment period
      tdata$FREQ = NA
      tdata$FREQ[which(regexpr("Q1W", tdata$ARMA)>0)] = "Q1W"    
      tdata$FREQ[which(regexpr("Q2W", tdata$ARMA)>0)] = "Q2W"
      tdata$FREQ[which(regexpr("Q3W", tdata$ARMA)>0)] = "Q3W"
      tdata$FREQ[which(regexpr("Q4W", tdata$ARMA)>0)] = "Q4W"
      tdata = filter(tdata, (tdata$FREQ=="Q1W" & tdata$TIME>=(T.END-7) & tdata$TIME<=(T.END)) |
                            (tdata$FREQ=="Q2W" & tdata$TIME>=(T.END-14) & tdata$TIME<=(T.END)) |
                            (tdata$FREQ=="Q3W" & tdata$TIME>=(T.END-21) & tdata$TIME<=(T.END)) |
                            (tdata$FREQ=="Q4W" & tdata$TIME>=(T.END-28) & tdata$TIME<=(T.END)) )  
      
      # calculate individual CMIN, CMAX, AUCtau
      n = 1:4
      FREQ_MULTIPLE = AUC12wk/n; 
      names(FREQ_MULTIPLE) = paste("Q",n,"W",sep="")
      
      RTAB = tdata %>% group_by(POP, F1, ARMA, FREQ, USUBJID) %>% dplyr::summarise(
             "CMIN" = min(DVOR, na.rm=TRUE), 
             "CMAX" = max(DVOR, na.rm=TRUE),
             "AUCtau"= auc_partial(TIME, DVOR)) %>% mutate(         
             "AUC12wk" = AUCtau * FREQ_MULTIPLE[as.character(FREQ)] ) %>%  gather(PARMS, DVOR, CMIN, CMAX, AUCtau, AUC12wk)

      return(RTAB)
  }
         


  #----------------------------------------------------------------------------- 
  # summary_key_parms
  #-----------------------------------------------------------------------------          
  summary_parms <- function(RTAB) {         
  # calculate the group mean of CMIN, MAX, AUCtau  
  POP = sort(unique(RTAB$POP))
  PARMS = sort(unique(RTAB$PARMS))  # c("CMIN", "CMAX", "AUCtau", "AUC12wk")    # , F1=  c("0.60", "0.65", "0.70", "0.75", "0.80")    
  ARMA =  sort(unique(RTAB$ARMA)) 
   
  lst = expand.grid(POP=POP, PARMS=PARMS)
  
  tt = lapply(1:nrow(lst), function(i, RTAB, lst) {
           tdata = RTAB %>% filter( POP==lst[i,"POP"] & as.character(PARMS)==lst[i,"PARMS"]) %>%          #, F1 ==lst[i, "F1"]
                mutate(NTIM = 1) %>% 
              calc_stats(id="USUBJID", group_by=c("ARMA"), value="DVOR", signif=TRUE ) %>% as.data.frame() %>% cbind(POP=as.character(lst[i,"POP"]), PARMS=as.character(lst[i,"PARMS"]))
              }, RTAB, lst) %>% bind_rows() %>% select(POP, PARMS, ARMA,  N, Mean_CV)
   
  tt$POP = ordered( tt$POP , level = POP)        
  tt$PARMS = ordered(tt$PARMS , level = PARMS)   
  tt$ARMA = ordered(tt$ARMA , level = ARMA) 
  tt = tt %>% arrange(POP, PARMS, ARMA) 
  
  tt = tt %>% spread(POP, Mean_CV)     # %>% kable()
  
  tt$ARMA = trim(gsub("INFUSION", "", tt$ARMA, fix=TRUE)  )
    
  return(tt)
  }
  
################################################################################  
################################################################################
#   adpp:  generate a set of population parameters
################################################################################  
################################################################################
  #-----------------------------------------------------------------------------
  # input:  n.subject;
  #        parms$  THETA, OMEGA, SIGMA
  #
  # output:
  #   1)  a population of individual model parameters
  #      if n.subject = 1 and parms$OMEGA = 0, then it outputs only one single 
  #      population-mean PK parameter
  #----------------------------------------------------------------------------- 
#' Extracts the time matched concntration vs effect data
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData)
  generate.pop.parms <- function(n.subject, parms) {
 
    require('MASS')
    #library('MASS') # , lib.loc = "H:\\TERR\\x86_64-pc-windows-library\\2.1")
  	ETA <- mvrnorm(n=n.subject,mu=rep(0,length(parms$THETA)),Sigma=parms$OMEGA)
  	if(is.vector(ETA))  ETA <- t(data.frame(ETA))
    
    #ETA <- ETA[, which(apply(ETA, 2, 'sum') != 0 )]
  	#if(is.vector(ETA))  ETA <- t(data.frame(ETA))
  	
    colnames(ETA) <- paste("ETA.", colnames(ETA), sep="")
    
    names(parms$THETA) <- paste("THETA", names(parms$THETA), sep=".")
    names(parms$SIGMA) <- paste("SIGMA", names(parms$SIGMA), sep=".")
      
  #  colnames(tt$OMEGA) <- rownames(tt$OMEGA) <- paste("OMEGA", colnames(tt$OMEGA), sep=".")  
  #  t1 = melt(tt$OMEGA)
  #  t1 = t1[which(t1[, 'value']!=0), ]
  
    indiv.parms = as.data.frame(c(parms$THETA, parms$SIGMA))
    #indiv.parms = indiv.parms[rep(1, times=n.subject), ]
  	if(is.vector(indiv.parms))  indiv.parms <- t(data.frame(indiv.parms))    
    indiv.parms = cbind(indiv.parms, ETA)
    
    indiv.parms$USUBJID <- u.add.prefix(1:n.subject, prefix="", add.number.zero=4)
    return(indiv.parms)
  }
 
 
  

#http://stackoverflow.com/questions/34365803/how-to-place-plus-minus-operator-in-text-annotation-of-plot-ggplot2
#        
# a <- ggplot()
#a <- a + geom_point(aes(x=seq(0,1,0.1), y=seq(0,1,0.1)))
#a <- a + annotate("text", x=0.5, y=0.3, label="myplot")
#a + annotate("text", x=0.5, y=0.2, label="\u00B1")
#
# a + annotate("text", x=0.5, y=0.2, label="?")
#  
#a0 <- ggplot()
#a0 <- a0 + geom_point(aes(x=seq(0,1,0.1), y=seq(0,1,0.1)))
#a0 + annotate("text", x=0.5, y=0.1, label="'' %+-% '' ", parse=TRUE)
#   
#The key idea is that %+-% is an operator, so it has to operate on something, i.e. it has to be in the form x %+-% y; in this case I've made x and y be blank strings.   
#   
    
    
    
    
     
  


  # in PKPDmisc
#  
#resample_df(df, key_cols, strat_cols = NULL, n = NULL,
#  key_col_name = "KEY", replace = TRUE)
#resampling
#
#resample_df(df, key_cols, strat_cols = NULL, n = NULL,
#  key_col_name = "KEY", replace = TRUE)
# Arguments
#df
#data frame
#key_cols
#key columns to resample on
#strat_cols
#columns to maintain proportion for stratification
#n
#number of unique sampled keys, defaults to match dataset
#key_col_name
#name of outputted key column. Default to "KEY"
#replace
#whether to stratify with replacement
#Details
#This function is valuable when generating a large simulated population where you goal is to create resampled sub-populations in addition to being able to maintain certain stratifications of factors like covariate distributions
#
#A new keyed column will be created (defaults to name 'KEY') that contains the uniquely created new samples. This allows one to easily compare against the key'd columns. Eg, if you would like to see how many times a particular individual was resampled you can check the original ID column against the number of key's associated with that ID number.

#Examples
  
    