# Version 0.1   Created on 11/08/2018, Feng Yang
# 
#----------------------------------------------------------------------------
  
  
  # Need 
  # 1) model file only
   
  
  if (idebug ==1) {
    FIGURE_ALL = NULL
    TABLE_ALL = NULL
  }
   
  #############################################################################
  #############################################################################
  # use potential dose regimens to simulate the PK profile for  
  # 1) a typical subject, or 
  # 2) a population of subjects
  
  # Note use non-individual parameters for potential dosing regimens of 
  # REGN3918 (800 mg SC QW)  
  #############################################################################
  #############################################################################
  
  
  # load mrgsolve cpp model
  #-------------------------------------
  library(dplyr)
  library("mrgsolve")
  source("./cpp/mod_R3918_human_MM_final.cpp")
  # 
  # 
  # source("./KRM/cpp/mod_R3918_human_MM.cpp")  
  # 
  # 
  # source("./KRM/cpp/sigmoid_human.cpp")
  mod <- mread("R2810", tempdir(),mymodel)     # ?mread , ?mcode
  
  
  
  ###################################
  # runCurrentSim
  ###################################
  runCurrentSim <- function(mod, 
                            adsl, 
                            Dosing.Regimen, 
                            infusion.hour = 1,
                            delta = 1,
                            followUpPeriod = 84, 
                            seed=1234) {
    
    adex = parseARMA(Dosing.Regimen) %>% rename(DOSEID = ID)
    
    tdata = expand.grid(DOSEID = unique(adex$DOSEID), USUBJID = unique(adsl$USUBJID))   #, POP=unique(adsl$POP))
    tdata = tdata %>% left_join(adex, by="DOSEID")
    tdata = tdata %>% left_join(adsl, by=c("USUBJID"  ))
    
    adex = tdata %>% mutate(ID = as.integer(as.factor(paste0(USUBJID,"_", DOSEID, "_", POP))),
                            amt = ifelse(unit=="mg/kg", amt * as_numeric(WGTBL), amt), 
                            cmt = ifelse(route=="IV", 2, 
                                         ifelse(route=="SC", 1, 0)), 
                            rate = ifelse(route=="IV", amt/(1/24), 0))
    
    adex = adex %>% rename(ARMA0=ARMA) %>% 
      group_by(ID) %>% mutate(ARMA = paste0(unique(ARMA0), collapse="+")) %>% ungroup() %>% as.data.frame()
    
    # set seed to make reproducible simulation (if IIV=1)
    set.seed(seed)
     
    
    # library("PKPDmisc")
    treat_end = max((adex$addl+1)*adex$ii)  #l24*7    
    sim_end = treat_end + followUpPeriod   # default 112 days                      # note dose by week only
    tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7), infusion.hour=infusion.hour)
    
    #---------------------------------------- 
    # run simulation
    #---------------------------------------- 
    out = mod  %>% data_set(as.ev(adex)) %>% 
      mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>%  as.data.frame()
    colnames(out) = toupper(colnames(out))
    
    # only add ID, STUDYID, USUBJID, ARMA
    out = out %>% left_join(adex %>% as.data.frame() %>% 
                              distinct(ID, .keep_all=TRUE) %>% 
                              select(ID,  USUBJID, ARMA, POP),
                            by=c("ID"))  
    
    # nominal time (NTIM)
    out = out %>% mutate(NTIM = TIME)
    
    simData = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% slice(2:n()) 
    
    
    # fill up II by  Last Observation Carried Forward
    # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
    #out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% group_by(ID) %>% fill(II)
    
    
    # EVID, DVOR and EXSEQ    
    # -----------------------------------
    #simData = out %>% mutate(EVID=ifelse(TAD==0, 1, 0))   %>% slice(2:n()) %>%
    #  mutate(DVOR = CP) %>% 
    #  group_by(ID)%>%  mutate(EXSEQ=cumsum(EVID)) %>%   # for calculat the last dose interval later
    #  as.data.frame()
    
    return(simData)
  }
  
   
  
  ##########################################################################################
  # canvas_simulation_results
  ##########################################################################################
  fig = empty_canvas(label="Simulation Results")
  attr(fig, 'title') <-  ""
  
  FIGURE_ALL[["canvas_simulation_results"]] = fig  
  
  
   
  
  
  
  seed = 1234
  
  ################################################################################################
  # priminary plots for a single typical subject with body weight 70 kg
  ################################################################################################
  dosing.regimen = c("400 mg SC QW*24" ,   
                     "800 mg SC QW*24"
  )
  
  dosing.regimen = c("30 mg/kg Q1W*1 IV + 400 mg SC QW*24" ,   
                     "30 mg/kg Q1W*1 IV + 800 mg SC QW*24"
  )
  
  
  # define a population
  nsubject = 1 
  adsl = create_adsl(POP="70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
  adsl1 = adsl %>% mutate(ID=1, USUBJID="0001",  
                          POP="Typical Subject", 
                          WGTBL = 70,    # min = 53.4, mean=70,  max=108.0 
                          C5BL = 80)       #  min=27.3   mean=80,   max=108.0
                          #CH5BL= 222)     # min=132, median=222, max = 394
  adsl2 = adsl %>% mutate(ID=2, USUBJID="0001", 
                          POP="Extreme Case", 
                          WGTBL = 108,    # min = 53.4, mean=70,  max=108.0 
                          C5BL = 108)   #  min=27.3   mean=80,   max=108.0
                          #CH5BL= 222)     # min=132, median=222, max = 394
  adsl = rbind(adsl1, adsl2)
  
  simData = runSim_by_dosing_regimen(mod  %>% zero_re %>% param( COVARIATE=1, SCALING=0, C5_ON_EMAX=0),   # %>% zero_re
                                     adsl, 
                                     dosing.regimen, 
                                     infusion.hour = 1,  # hour,  infusion hours
                                     delta = 1,
                                     followup.period = 84, 
                                     seed=1234)
  
  simData = simData  %>% mutate(DVOR=CENTRAL/V2, 
                                POP=ordered(POP, levels=c("Typical Subject", "Extreme Case")))
  
  #---------------------------------------- 
  # priminary PK plot     
  # ---------------------------------------
  tdata = simData
  x=setup_axis(xscale='7_28', xlimits=c(0, max(tdata$TIME)))
  y=setup_axis(xscale='50_100', xlimits=c(0, max(tdata$DVOR)))
  
  
  fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
    facet_wrap(~POP) + 
    
    xlab("Time (day)") + 
    ylab("Predicted Conc. of Total REGN3918 (mg/L)") + 
    
    #coord_cartesian(xlim = c(0, 85)) + 
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    scale_y_continuous(breaks=y$breaks, label=y$labels) +
  
    guides(col=guide_legend(ncol=2,byrow=TRUE))    
  
  attr(fig, 'title') <-   "Predicted Mean Concentration-time Profiles of REGN3918 for Potential Dosing Regimens"
  FIGURE_ALL[["priminary_PK_plot"]]  = fig 
  
  fig
  
    
  # priminary PD plot     
  # -----------------------------------
  tdata = simData   %>% mutate(CH50_PCHG = (CH50-CH50HBL)/CH50HBL*100) %>% mutate(DVOR=CH50_PCHG)
  x=setup_axis(xscale='7_28', xlimits=c(0, max(tdata$TIME)))
  
  fig = ggplot(data=tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_line() + 
    facet_wrap(~POP) + 
    
    xlab("Time (day)") + 
    ylab("Predicted Percent Change from Baseline (CH50)") + 
    
    #coord_cartesian(xlim = c(0, 85)) + 
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
    
    guides(col=guide_legend(ncol=2,byrow=TRUE))   
  
  fig
  
  attr(fig, 'title') <-   "Predicted Mean Percent Change from Baseline (CH50) for Potential Dosing Regimens"
  #FIGURE_ALL[["priminary_PD_plot"]]  = fig 
   
    
  # CH50HBL = 222
  # EMAX = 201
  # GAMMA = 6.86
  # E50 = 20.7
  # CP = seq(0, 100, by=1)
  # 
  # CH50 = CH50HBL - EMAX*CP^GAMMA/(EC50^GAMMA+CP^GAMMA)
  # 
  # plot(CP, CH50)
  
   
  
   
  
  ################################################################################################
  # priminary plots for typical pediatric patients
  ################################################################################################
  # Omitted. 
    
  
  ################################################################################################
  # Plot 95% CI time profiles  of both PK and PD
  ################################################################################################
  

  
  dosing.regimen = c("400 mg SC QW*24" ,   
                     "800 mg SC QW*24"
  )
  
  dosing.regimen = c("30 mg/kg Q1W*1 IV + 400 mg SC QW*24" ,   
                     "30 mg/kg Q1W*1 IV + 800 mg SC QW*24"
  )
  
  seed=1234
  nsubject = 1000 
  adsl = create_adsl(POP="70kg", nsubject, meanWt=70, seed =seed) #%>% filter(WGTBL<160)
  adsl1 = adsl %>% mutate(USUBJID= add_prefix(1:nsubject, digits=4),  
                          POP="Typical Subject", 
                          WGTBL = 70,      # min = 53.4, mean=70,  max=108.0 
                          C5BL = 80,       #  min=27.3   mean=80,   max=108.0
                          CH5BL= 222)      # min=132, median=222, max = 394
  adsl2 = adsl %>% mutate(USUBJID=add_prefix((nsubject+1):(nsubject*2), digits=4),   # "0001", 
                          POP="Extreme Case", 
                          WGTBL = 108,    # min = 53.4, mean=70,  max=108.0 
                          C5BL = 80,     #  min=27.3   mean=80,   max=108.0
                          CH5BL= 222)     # min=132, median=222, max = 394
  adsl = rbind(adsl1, adsl2) %>% mutate(POP=ordered(POP, levels=c("Typical Subject", "Extreme Case")))
  
  
  simData = runSim_by_dosing_regimen(mod  %>% param( COVARIATE=1, SCALING=0),      #%>% zero_re
                                     adsl, 
                                     dosing.regimen, 
                                     infusion.hour = 1,  # hour,  infusion hours
                                     delta = 1,
                                     followup.period = 84, 
                                     seed=1234)
  
  simData1 = simData  # no loading dose, with C5 and Bodyweight
  simData2 = simData  # with loading dose, with C5 and Bodyweight
  
  simData3 = simData  # no loading dose, with normal C5 and extreme Bodyweight as Tom suggested
  simData4 = simData  # with loading dose, with normal C5 and extreme Bodyweight as Tom suggested
  
  # PK 
  
  plot.fig <- function(simData) {
      tdata = simData   %>% mutate(DVOR = CENTRAL/V2, NTIM=TIME)  #%>% filter(POP=="Typical Subject")
       
      tdata = tdata %>% calc_stats(id="USUBJID", group_by=c("POP", "ARMA", "NTIM"), value="DVOR") 
      
      
      #---------------------------------------------------------------------------------------------------
     
      x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$NTIM/7, na.rm=TRUE)))
      y=setup_scale(myscale='100_200', mylimit=c(0, 1000))
      
      #tdata = tdata %>% filter(POP=="Typical Subject")
      fig = ggplot(tdata %>%mutate(NTIM=NTIM/7)  , aes(x=NTIM, y=Mean, group=ARMA, color=ARMA )) +
        
        geom_line() + 
        #facet_wrap(~POP) + 
        scale_color_manual(values=colScheme()) + 
      
        #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=2)  + 
        geom_ribbon(data=tdata %>%mutate(NTIM=NTIM/7),aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3) + 
        
        #facet_wrap(~TEST, nrow=3, scales="free" ) + 
        scale_x_continuous(breaks=x$breaks, label=x$labels) +
        scale_y_continuous(breaks=y$breaks, label=y$labels) +
        
        #ggtitle("Concentration Time Profile") + 
        xlab("Time (week)") + 
        ylab("Concentration of Total Pozelimab (mg/L)") + 
        #scale_y_log10() + 
        #coord_cartesian(xlim = c(0, 85)) + 
        theme_bw() + base_theme()  +
        #scale_colour_manual(values = c("black", "blue", "green")) + 
         
        base_theme(font.size = as.integer(12)) + 
        guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
         
      
       geom_hline(yintercept=c(100), lty="dashed") + 
        geom_text(x=30, y=115, aes(label="100 mg/L", hjust=0),   color='black')# + 
         
         #guides(color=guide_legend(override.aes=list(fill=NA)))
       
      fig = fig +   facet_wrap(~POP)   #+ ggplot2::theme(legend.position = "none")
      #fig
      
      attr(fig, 'title') <- "Predicted Mean Concentration-Time Proifiles (95% CI, Linear-scale) for Potential Dosing Regimens" 
      attr(fig, 'width') <- 10
      attr(fig, 'height') <- 6
      
      return(fig)
  }
  
  
    
  FIGURE_ALL[["PK_TIME_PROFILE_CI95_LN_fig1"]]  = plot.fig(simData1) 
  FIGURE_ALL[["PK_TIME_PROFILE_CI95_LN_fig2"]]  = plot.fig(simData2) 
  FIGURE_ALL[["PK_TIME_PROFILE_CI95_LN_fig3"]]  = plot.fig(simData3) 
  FIGURE_ALL[["PK_TIME_PROFILE_CI95_LN_fig4"]]  = plot.fig(simData4) 
  
  # 
  # fig = fig +  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
  #                 labels =   trans_format("log10", math_format(10^.x))) 
  # fig = fig + annotation_logticks(sides ="l") +  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
  #   theme(panel.grid.minor = element_blank()) + ggplot2::theme(legend.position = "none")
  #  
  # attr(fig, 'title') <-  "Predicted Mean Concentration-Time Proifiles (95% CI, Log-scale) for Potential Dosing Regimens"
  # attr(fig, 'width') <- 8
  # attr(fig, 'height') <- 6
  # FIGURE_ALL[["PK_TIME_PROFILE_CI95_LOG"]]  = fig 
   
  
  
  #------------------------------
  # PD
  #------------------------------
  
  tdata = simData   %>% mutate(DVOR = (CH50-CH50HBL)/CH50HBL*100, NTIM=TIME)
  
  tdata = tdata %>% calc_stats(id="USUBJID", group_by=c("POP", "ARMA", "NTIM"), value="DVOR") 
   
  x=setup_axis(xscale='7_28', xlimits=c(0, max(tdata$NTIM)))
  #y=setup_axis(xscale='100_200', xlimits=c(0, 1000))
  
  fig = ggplot(tdata , aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
    # geom_point() + 
    geom_line() + 
    #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=2)  + 
    geom_ribbon(data=tdata,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3) + 
    
    #facet_wrap(~TEST, nrow=3, scales="free" ) + 
    scale_x_continuous(breaks=x$xbreaks, label=x$xlabels) +
    #ggtitle("Concentration Time Profile") + 
    xlab("Time (day)") + 
    ylab("Mean Percent Change from Baseline in CH50") + 
    #scale_y_log10() + 
    #coord_cartesian(xlim = c(0, 85)) + 
    theme_bw() + 
    #scale_colour_manual(values = c("black", "blue", "green")) + 
    
    ggplot2::theme(legend.position = "bottom", legend.title = element_blank()) + 
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
    
    ggplot2::theme(  #panel.grid.minor = element_line(colour = "gray95",size=0.75), 
      panel.grid.major = element_line(colour = "gray97",size=0.75)  )  + 
    
    guides(color=guide_legend(override.aes=list(fill=NA))) 
    
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)),
    #              labels =   trans_format("log10", math_format(10^.x))
    #) 
  
  #fig = fig + annotation_logticks(sides ="l") +  facet_wrap(~ARMA) + # "trbl", for top, right, bottom, and left.
  #  theme(panel.grid.minor = element_blank())
  fig = fig  +facet_wrap(~POP)  #+ ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <-  "Predicted Mean Concentration-Time Proifiles (95% CI) for Potential Dosing Regimens"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  #FIGURE_ALL[["CH50PCT_TIME_PROFILE_CI95"]]  = fig 
  
  
  
  
  
  
  ################################################################################################
  # calcualte individual exposure metrics
  ################################################################################################
  
  # PK 
  # tdata = simData   %>% filter(TIME==35)  %>% mutate(DVOR = CP)
  # tabl = tdata  %>%  calc_stats(id="USUBJID", group_by=c("POP", "ARMA"), value="DVOR") %>% 
  #   select(group_by, N, Mean_SE, Mean_SD, Median_Range)
  # 
  # attr(tabl, 'title') <- "Summary of Concentration for Different Dosing Regimens at Day 35"
  # TABLE_ALL[["Summary_PK_Day35"]]  = tabl
  #  
  # 
  # # CH50_PCHG
  # tdata = simData_70kg_N1000   %>% filter(TIME==35)  %>% mutate( CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100, 
  #                                                                DVOR = CH50_PCHG)
  # tabl = tdata  %>%  calc_stats(id="USUBJID", group_by=c("POP", "ARMA"), value="DVOR") %>% 
  #   select(group_by, N, Mean_SE, Mean_SD, Median_Range)
  # 
  # attr(tabl, 'title') <- "Summary of Percentage of Change from Baseline (CH50) for Different Dosing Regimens at Day 35" 
  # TABLE_ALL[["Summary_CH50_PCHG_Day35"]]  = tabl
  
   
  
  # filter out the last dosing interval
  #tdata = tdata  %>%  
   # group_by_(.dots =c(group_by )) %>%  
   # top_n(n=1,wt=EXSEQ) %>% filter(TAD<=II)
  #
  
  # calculate the PK parameter for each subject and each time interval
  # out = tdata  %>% 
  #   group_by_(.dots =c(group_by )) %>%   
  #   dplyr::summarise(
  #     #CMIN = round(min(DVOR),digits=3),    # Note the infusion hours, could be 5 min to 2 hr, typically.
  #     CMIN = round(DVOR[n()],digits=3),   # the last conc at this time interval
  #     CMAX = round(max(DVOR),digits=3),       
  #     AUCtau = round(auc_partial(NTIM, DVOR),digits=3)    # , data%>%pull(input$yvar))   #, range=data%>%range_(input$xvar)
  #   )  
  
  
  
  
  
  
  ################################################################################################
  # calculate the percentage of patient above conc threshold at treatmetn period
  ################################################################################################
  
  if (1==2)  {
  tdata = simData 
  
  tdata = tdata %>% mutate(CH50_PCHG = (CH50-CH50_BL)/CH50_BL*100)
  
  tdata = tdata %>% filter(TIME==35)  %>% 
          mutate(CONC_GT_100 = ifelse(CP>=100, 1, 0), 
                 CONC_GT_150 = ifelse(CP>=150, 1, 0), 
                 CONC_GT_200 = ifelse(CP>=200, 1, 0),
                 
                 CH50_EC99 = ifelse(CH50_PCHG<(-99), 1, 0),
                 CH50_EC95 = ifelse(CH50_PCHG<(-95), 1, 0), 
                 CH50_EC90 = ifelse(CH50_PCHG<(-90), 1, 0), 
                 CH50_EC80 = ifelse(CH50_PCHG<(-80), 1, 0)
  )
  
  # percentage of population above theshold concentration  
  tabl = tdata %>% group_by(ARMA) %>% summarise(N=length(unique(USUBJID)), 
                                         PCT_CONC_GT_100 = paste0(round(sum(CONC_GT_100)/N*100,digits=2),"%"),
                                         PCT_CONC_GT_150 = paste0(round(sum(CONC_GT_150)/N*100,digits=2),"%"),
                                         PCT_CONC_GT_200 = paste0(round(sum(CONC_GT_200)/N*100,digits=2),"%")
  )
  
  
  attr(tabl, 'title') <- "Percentage of Subject Achieving threshold Concentration for Different Dosing Regimens at Day 35" 
  TABLE_ALL[["CONC_PCT_Day35"]] = tabl
  
  tabl 
  
  # percentage of population achieving EC99
  tabl = tdata %>% group_by(ARMA) %>% summarise(N=length(unique(USUBJID)), 
                                         PCT_CH50_EC99 = paste0(round(sum(CH50_EC99)/N*100,digits=2),"%"),
                                         PCT_CH50_EC95 = paste0(round(sum(CH50_EC95)/N*100,digits=2),"%"),
                                         PCT_CH50_EC90 = paste0(round(sum(CH50_EC90)/N*100,digits=2),"%"), 
                                         PCT_CH50_EC80 = paste0(round(sum(CH50_EC80)/N*100,digits=2),"%")
  )
  
  attr(tabl, 'title') <-"Percentage of Subject Achieving EC80 to EC99 for Different Dosing Regimens at Day 35" 
  
  TABLE_ALL[["CH50_PCT_Day35"]] = tabl
   
   
  
  tdata %>% group_by(ARMA, CH50_EC99) %>% summarise(N=length(unique(USUBJID)), 
                                                    Mean=mean(CP)) %>% filter(CH50_EC99==1) %>% as.data.frame()
  
  
  FIGURE_ALL[["tab_pk_parameters"]]  = fig
  
  
  }
  
  
  ################################################################################################
  # final output to doc and ppt
  ################################################################################################
  
  if (idebug ==1) { 
    # use customized version
    myppt <- pptx(title = "title", template = './KRM/docs/pptTemplate.pptx')
    mydoc <- docx(template = "./KRM/docs/memoTemplate.docx", empty_template = FALSE)
    
    tt = print2_word_ppt(FIGURE_ALL, TABLE_ALL,  mydoc, myppt) 
    
    writeDoc(tt$mydoc, file = './KRM/docs/memo_results.docx')
    
    writeDoc(tt$myppt, file = './KRM/docs/ppt_results.pptx')
    
  }
  
  



