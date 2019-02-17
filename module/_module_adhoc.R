 
   
  # for C5 adhoc requests   from sumit
  ######################################################################
  # standalize the adsl
  ######################################################################
  
  # USUBJID
  #--------------

  fun.adsl.checkin <- function(adsl) { 
    colnames(adsl) = gsub(" ", "_", toupper(colnames(adsl)), fix=TRUE)
    
  adsl$SUBJECT = adsl$SUBJECT_NUMBER
  adsl$SUBJECT = paste(substr(adsl$SUBJECT, 1, 3), substr(adsl$SUBJECT, 4, 6), substr(adsl$SUBJECT, 7, 9), sep="-")
  adsl$USUBJID = paste("R3918-HV-1659", adsl$SUBJECT, sep="-")
  
  # ARMA
  #-----------------------
  adsl$ARMA = gsub("REGN3918 at ", "", adsl$COHORT, fix=TRUE)
  adsl$ARMA = gsub(", single dose", "", adsl$ARMA, fix=TRUE)
  adsl$ARMA[which(adsl$TREATMENT_GROUP=="Placebo")] = "Placebo"
  adsl$ARMA 
  
  adsl$WEIGHTBL = adsl$SUBJECT_WEIGHT_IN_KG
  
  
  adsl = adsl %>% select(USUBJID, COHORT, ARMA, WEIGHTBL, RANDOMIZATION_DATE, EARLY_TERMINATION_DATE, RANDOMIZATION_NUMBER)
  return(adsl)
  }
  
  
  ######################################################################
  # standalize the adpc
  ######################################################################
  
  fun.adpd.checkin <- function(adpd, adsl) { 
    tdata = adpd
    colnames(tdata) = gsub(" ", "_", toupper(colnames(tdata)), fix=TRUE)
  
  tdata = tdata %>% filter(TEST_NAME=="CH50 PD" )
  
  tdata$STUDYID = "R3918-HV-1659"
  tdata$TEST = tdata$TEST_NAME  
  tdata$STDUNIT = tdata$UNIT
  tdata$BLQ = "."
  tdata$LLOQ = "."
  tdata$SOP = "."
  
  
  # USUBJID
  #--------------
  tdata$SUBJECT = tdata$PATIENT_ID
  tdata$SUBJECT = paste(substr(tdata$SUBJECT, 1, 3), substr(tdata$SUBJECT, 4, 6), substr(tdata$SUBJECT, 7, 9), sep="-")
  tdata$USUBJID = paste("R3918-HV-1659", tdata$SUBJECT, sep="-")
  
  # ARMA
  #-----------------------
  tdata = tdata %>% left_join(adsl%>%select(USUBJID, ARMA), by="USUBJID") 
  
  
  # VISIT and TIMEPT
  #---------------------
  tmp = strsplit(tdata$VISIT_NAME, "/") # %>% unlist()
   
  nMax <- max(sapply(tmp, length))
  dat <-  t(sapply(tmp, function(i) i[1:nMax])) %>% as.data.frame()
  colnames(dat) = c("VISIT", "TIMEPT")
  tdata = bind_cols(tdata, dat)
  tdata$VISIT = as.character(toupper(tdata$VISIT))
  
  #tdata %>% select(USUBJID, ARMA, VISIT_NAME, VISIT, TIMEPT)
   
  
  # NTIM
  #-------------------
  # from protocol table, footnote 9
  #On day 1/baseline, PK and PD assessments (CH50, AH50, Total C5) should be drawn 
  #pre-dose, at the end of the infusion or injection, and at 
  #15 min, 30 min, 1, 2, 4, 8, 12, and 24 hours after the end of the infusion or injection as detailed in Table 2.
  
  unique(tdata$VISIT)
  
  # TIMEPT_TIME
  TIMEPT = c("PRE",  "EOI",  "15MIN",    "30MIN",   "1HR",   "2HR",  "4HR",   "8HR",  "12HR")
  NTIM = c(-15/60/24, 0, 15/60/24, 30/60/24, 1/24, 2/24, 4/24, 8/24, 12/24)
  TIMEPT_NTIM = data.frame(TIMEPT, NTIM); rownames(TIMEPT_NTIM) = TIMEPT_NTIM$TIMEPT
  
  # VISIT_NTIM
  VISIT = paste0("VISIT ", c(1,   3,  4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
  NDAY =                  c(-56, -14, 0, 1, 3, 7, 14, 21, 28, 42, 56, 84, 98,-99)
  VISIT_NDAY = data.frame(VISIT, NDAY, stringsAsFactors = FALSE); rownames(VISIT_NDAY) = VISIT_NDAY$VISIT
  
  tdata = tdata %>% left_join(VISIT_NDAY, by="VISIT")
   
  
  tdata = tdata %>% left_join(TIMEPT_NTIM, by="TIMEPT") %>% mutate(NTIM=ifelse(is.na(NTIM), 0, NTIM)) #= TIMEPT_TIME[as.character(tdata$TIMEPT), "TIME"] 
  tdata$NTIM = tdata$NDAY + tdata$NTIM
  
  #tdata = tdata %>% mutate(NTIM=ifelse(is.na(NTIM), 0, NTIM))                                                                                                 
  
  tdata = tdata %>% arrange(USUBJID, NTIM)                                                                                                  
  
  
  # SAMDTTM
  #------------------
  tdata$SAMDTTM = tdata$COLLECTION_DATE
  tdata$SAMDTTM = as.POSIXct(as.character(tdata$SAMDTTM), format = "%d-%b-%Y", tz="GMT")   # sampling time     # if use Excel file  %m: %b: JUN
  #adpc$SAMDTTM = as.POSIXct(adpc$SAMDTTM, origin="1960-01-01", tz="GMT")
  
  # use VISIT 5 as a baselien to calculate the SAMDTTM for VISIT 4
  tt= tdata %>% filter(VISIT=="VISIT 5") %>% select(USUBJID, VISIT, SAMDTTM) %>% mutate(VISIT="VISIT 4", SAMDTTM=SAMDTTM-1*24*60*60)
  t1 = left_join(tdata %>% filter(VISIT=="VISIT 4") %>% select(-SAMDTTM), tt, by=c("USUBJID", "VISIT"))
  
  tdata = bind_rows(tdata %>% filter(VISIT!="VISIT 4"), t1)
   
  
 # tdata = tdata %>% select(USUBJID, ARMA, VISIT, TIMEPT, NDAY, NTIM, SAMDTTM, TEST_NAME, TEST_RESULT, UNITS ) %>% as.data.frame()
  
  # calculate percent of change
  #------------------------------------
  tt = tdata %>% filter(TIMEPT=="PRE") %>% distinct(USUBJID, .keep_all=TRUE) %>% 
    mutate(BASE=as_numeric(TEST_RESULT)) %>% select(USUBJID, BASE)
  tdata = left_join(tdata, tt, by="USUBJID")
  
    
  tdata = tdata %>% group_by(USUBJID) %>% 
                    mutate(DVOR = as_numeric(TEST_RESULT), 
                           CHG = DVOR-BASE, 
                           PCHG = round(CHG/BASE*100, digits=5))
  

  tdata = tdata %>% arrange(USUBJID, NTIM)

  tdata$TIME = tdata$NTIM  ################
  
  
  tdata0 = tdata   
  tdata0 %>% select(USUBJID, ARMA, VISIT_NAME, VISIT, TIMEPT, SAMDTTM, NTIM, TEST_NAME, DVOR, BASE, CHG, PCHG ) %>% as.data.frame()
  
  return(tdata0)
  }
  
  # https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows
  # mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  #   condition <- eval(substitute(condition), .data, envir)
  #   condition[is.na(condition)] = FALSE
  #   .data[condition, ] <- .data[condition, ] %>% mutate(...)
  #   .data
  # }
  # 
  # 
  
######################################################################
# for individual plot
######################################################################
  
  if (1==2) { 
  adsl   = fun.adsl.checkin(adsl.v0) 
  tdata0 = fun.adpd.checkin(adpd.v0, adsl)
    
    
  tdata = tdata0 %>% filter(!is.na(DVOR))
  
  tdata = tdata %>% select(USUBJID, ARMA, VISIT, TIMEPT, SAMDTTM, NTIM, TEST_NAME, DVOR, BASE, CHG, PCHG ) %>% as.data.frame()
   
  tdata$TIME = as_numeric(tdata$NTIM) ;  range(tdata$TIME)
  tdata$DVOR = as_numeric(tdata$PCHG);  range(tdata$DVOR)
  
  x = tick_label(x=tdata$TIME, xmin=-56, xmax=50, major.tick=7, minor.tick=1, log=FALSE)
  y = tick_label(x=tdata$DVOR, xmin=1E-2, xmax=200,  log=FALSE) 
  
  xmax = max(tdata$TIME,na.rm=TRUE)
  ymax = max(tdata$DVOR,na.rm=TRUE)
  ggplot(tdata , aes(x=TIME, y=DVOR, group=USUBJID, col=as.factor(ARMA), label=USUBJID)) + 
    geom_point() + 
    #geom_text(show.legend = FALSE) + 
    geom_line(lwd=0.5)  + 
    xlab("Time (day)") + 
    ylab("Percent Change from Baseline (CH50, U/mL)") + 
    
    #scale_x_continuous(breaks=c(0, 1, 3, 7, 14, 21, 28)) +    # breaks=x$breaks, label=x$labels) + 
    scale_x_continuous(breaks=x$breaks, label=x$labels) + 
    scale_y_continuous(breaks=c(-100,-80,-60,-40,-20,0,20,40)) +
    #scale_y_log10(breaks=y$breaks, label=y$labels) + 
    
    #coord_cartesian(xlim = c(0, 21)) + # range(tdata$TIME, na.rm=TRUE)) + 
    #coord_cartesian(ylim = range(tdata$DVOR, na.rm=TRUE)) + 
    
    base_theme(font.size = 14) + 
    theme(legend.title = element_blank())  + 
    theme(panel.grid.major = element_line(colour = "gray95",size=0.25))   
    
    #facet_wrap(~ARMA) + #scale_y_log10()  +       #+ scale_y_log10()
    
    #geom_point(data=tdata, aes(x=TIME, y=DVOR, group=USUBJID), col="blue")        # what if two datasets have different ARMA (legend)?
    
  
  ######################################################################
  # for mean plot
  ######################################################################
  
  tdata = tdata0 %>% select(USUBJID, ARMA, VISIT, TIMEPT, SAMDTTM, NTIM, TEST_NAME, DVOR, BASE, CHG, PCHG ) %>% as.data.frame()
  tdata = calc_stats(tdata, group_by=c("ARMA", "NTIM"), value="PCHG")
  
  
  
  tdata$TIME = as_numeric(tdata$NTIM) ;  range(tdata$TIME)
  tdata$DVOR = as_numeric(tdata$Mean);  range(tdata$DVOR)
  
  x = tick_label(x=tdata$TIME, xmin=-56, xmax=50, major.tick=7, minor.tick=1, log=FALSE)
  #y = tick_label(x=tdata$DVOR, xmin=1E-2, xmax=200,  log=FALSE) 
  
  xmax = max(tdata$TIME,na.rm=TRUE)
  ymax = max(tdata$DVOR,na.rm=TRUE)
  fig = ggplot(tdata , aes(x=TIME, y=DVOR, group=ARMA, col=as.factor(ARMA), label=ARMA)) + 
  geom_point() + 
  #geom_text(show.legend = FALSE) + 
  geom_line(lwd=0.5)  + 
  xlab("Time (day)") + 
  ylab("Percent Change from Baseline (CH50)") + 
  
  scale_x_continuous(breaks=x$breaks, label=x$labels) + 
  #scale_y_log10(breaks=y$breaks, label=y$labels) + 
  
  coord_cartesian(xlim = c(0, 21)) + # range(tdata$TIME, na.rm=TRUE)) + 
  #coord_cartesian(ylim = range(tdata$DVOR, na.rm=TRUE)) + 
  
  base_theme(font.size = 14) + 
  theme(legend.title = element_blank())  + 
  theme(panel.grid.major = element_line(colour = "gray95",size=0.25))    
  
  #facet_wrap(~ARMA) + #scale_y_log10()  +       #+ scale_y_log10()
  
  #geom_point(data=tdata, aes(x=TIME, y=DVOR, group=USUBJID), col="blue")        # what if two datasets have different ARMA (legend)?
  
  fig2 = fig + geom_errorbar(data=tdata, aes(ymin = meanMinusSD, ymax = meanPlusSD), width=0.5) 
  fig2
  
  
  }
  
  
  
