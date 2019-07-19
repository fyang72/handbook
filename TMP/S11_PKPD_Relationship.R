  #----------------------------------------------------------------------------
  # Version 0.1   Created on 11/08/2018, Feng Yang
  # 
  #----------------------------------------------------------------------------
  # Input
  # -----------------
  # 1) nmdat:  meta data file, or 
  # 2) PKPD:  dataset in a holizontal format
  #
  # Output Figures: 
  #------------------
  # 1) Sigmoid PKPD plot
  # 2) Hysterisis plot, indiv and mean plot
  # 3) Double-Y axis plot, indiv and mean plot 
  # 4) PD vs time by quantile of exposure
  #----------------------------------------------------------------------------
  # 
  
  if (idebug ==1) {
    FIGURE_ALL = NULL
    TABLE_ALL = NULL
    LISTING_ALL = NULL
  }
 
  ##########################################################################################
  # load nmdat for all subsequent analysis 
  ##########################################################################################
  nmdat = read_csv("./data/nmdat_PKPD_0118_2019.csv", 
                   col_type=cols(.default=col_character()))     # read as character as defualt
  
  arma.lst <- c("Placebo",  "1 mg/kg IV" ,  "3 mg/kg IV",    "10 mg/kg IV" ,   "30 mg/kg IV" , 
                "600 mg SC" ,  "300 mg SC" ,  
                "400 mg SC QW" )
   
  # make ARMA as a ordered factor
  nmdat = nmdat %>%       
    mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE),
           ARMA = ordered(ARMA, levels=arma.lst),
           ARMAN = as.integer(ARMA), 
           DVOR = as_numeric(DVOR), 
           TIME = as_numeric(TIME), 
           NTIM = as_numeric(NTIM),
           
           LLOQ = as_numeric(LLOQ)
    )%>% 
    filter(as_numeric(TIME)>=0)   # Do we want to exclude all pre-dose sampels, as the default?
   
  
  # spead it as holizontal, PKPD is the main source dataset
  tdata = nmdat %>% filter(TEST %in% c(PK.TEST.NAME, PD.TEST.NAME)) %>%   filter(!is.na(DVOR),   !is.na(TIMEPT))  %>% 
    select(USUBJID, ARMA, TIMEPT, NTIM, TEST, PCHG, DVOR, CH50HBL) %>% 
    mutate(DVOR = ifelse(TEST==PD.TEST.NAME, PCHG, DVOR))   
  
  tdata = tdata %>% select(-PCHG) %>%  spread(TEST, DVOR)
  
  tdata = tdata %>% mutate(ARMA = ordered(ARMA, levels = levels(unique(tdata$ARMA)) )) %>% 
    rename(PCHG = CH50H) %>% 
    mutate(PCHG = as_numeric(PCHG), 
           REGN3918 = as_numeric(REGN3918))  %>% 
    mutate(REGN3918 = ifelse(REGN3918==0|is.na(REGN3918), 0.039, REGN3918))
  
  PKPD = tdata %>% mutate(NTIM=as_numeric(NTIM), 
                          PK = as_numeric(REGN3918),
                          PD = as_numeric(PCHG)
                          ) %>% arrange(ARMA, USUBJID, NTIM)
  
  PKPD = PKPD %>% 
    mutate(USUBJID = gsub("R3918-HV-1659-", "", USUBJID, fix=TRUE)) %>% 
    mutate(USUBJID = ordered(USUBJID, levels=unique(USUBJID)))
  ##########################################################################################
  # PKPD Sigmoid Scatter plot [ use percent change from baseline]
  ##########################################################################################
  fig = empty_canvas(label="PKPD-Relationship"); 
  attr(fig, 'title') <- ""
  FIGURE_ALL[["canvas_PKPD_Relationship"]] = fig  
  
  
  
  tdata = PKPD %>% group_by(ARMA, USUBJID) %>% 
    mutate(xend = lead(PK,n=1),  # c(tail(Mean_PK, n=-1), NA)
           yend = lead(PD,n=1)   # c(tail(Mean_PD, n=-1), NA)
    )%>% fill(xend = xend, 
              yend = yend)
  
  library(stringr)
  
  #x=setup_axis(xscale='7_14', xlimits=range(tdata$NTIM, na.rm=TRUE))
  
  fig = ggplot(tdata , aes(x=PK, y=PD, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  #geom_line() +    
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    
    
    #coord_cartesian(ylim=c(0.8, 1E+3)) + 
    #scale_y_continuous(labels=fancy_scientific)  + 
    
    #scale_x_continuous(breaks=x$breaks, label=x$labels) +
    
    xlab(paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)")) + 
    ylab(paste0("Percent Change from Baseline in ", PD.TEST.LABEL, " (%)")) + 
    #coord_cartesian(xlim = c(0, 85)) + 
    
    theme_bw() +   
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  
  fig = fig + annotation_logticks(sides ="b") +  # "trbl", for top, right, bottom, and left.
    theme(panel.grid.minor = element_blank())
  fig
  
  fig0 = fig
  
  
  #-------------------------------------------------------------
  # addon-1: simulated profiles
  #-------------------------------------------------------------
  EC50 = 20.7
  GAMMA = 6.86  
  IMAX = 201
  
  # tdata = tdata %>% mutate(CONC = PK,   # 10^(seq(1E-1, 1E+3, by=0.1)), 
  #                          CH50_SIM = CH50HBL - CH50HBL*CONC^GAMMA/(EC50^GAMMA + CONC^GAMMA), 
  #                          CH50_PCHG_SIM = (CH50_SIM-CH50HBL)/CH50HBL*100
  # ) %>% arrange(CONC)

  #FIGURE_ALL[["PK_CH50_PCHG_log_LN_with_simulation"]] = 
  # fig =  fig +
  #  geom_line(data=tdata%>%arrange(CONC), aes(x=CONC, y=CH50_PCHG_SIM), col="black", lwd=1)
  
  
  attr(fig, 'title') <-  "Sigmoid PK-PD (Percent Change from Baseline) Relationship Regardless of Time Points and Dose Groups [Semi-Log Scale]"
  
  FIGURE_ALL[["SIGMOID_PKPD_PCHG_LOG"]] = fig
  
  fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <- "Sigmoid PK-PD (Percent Change from Baseline) Relationship Regardless of Time Points and Dose Groups [Semi-Log Scale]"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["SIGMOID_PKPD_PCHG_LOG_PANEL"]] = fig 
  
  
  
  
  #-------------------------------------------------------------
  # addon-2: individual hysteresis plot, if N is small 
  #-------------------------------------------------------------
  fig = fig0 + facet_wrap(~ARMA) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig
   
  
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["HYSTERESIS_INDIV_PCHG_LOG_BY_ARMA"]] = fig 
  
  # by USUBJID
  fig = fig0 + facet_wrap(~USUBJID) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig
  
  
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 12
  attr(fig, 'height') <- 9
  FIGURE_ALL[["HYSTERESIS_INDIV_PCHG_LOG_BY_USUBJID"]] = fig 
  
  
  ##########################################################################################
  # Mean hysteresis plot  
  ##########################################################################################
  
  # data: 
  #---------- 
  tdata.PK = PKPD %>% calc_stats(group_by=c("ARMA", "TIMEPT", "NTIM"), value="PK") %>% 
    select(ARMA, TIMEPT,NTIM, Mean, Median) %>% 
    rename(Mean_PK=Mean, 
           Median_PK = Median)
  
  tdata.PD = PKPD %>% calc_stats(group_by=c("ARMA", "TIMEPT", "NTIM"), value="PD") %>% 
    select(ARMA, TIMEPT, NTIM,Mean, Median) %>% 
    rename(Mean_PD=Mean, 
           Median_PD = Median)
   
  tdata = tdata.PK %>% left_join(tdata.PD, by=c("ARMA", "TIMEPT", "NTIM")) %>% 
    arrange(ARMA, NTIM)
  
  
  tdata = tdata %>% group_by(ARMA) %>% 
    mutate(xend = lead(Mean_PK,n=1),  # c(tail(Mean_PK, n=-1), NA)
           yend = lead(Mean_PD,n=1)   # c(tail(Mean_PD, n=-1), NA)
    )%>% fill(xend = xend, 
              yend = yend)
    
  # plot
  #------- 
  fig = ggplot(tdata , aes(x=Mean_PK, y=Mean_PD, col=ARMA, group=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +    
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    #scale_x_continuous(breaks=x$breaks, label=x$labels) +    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    
    
    #coord_cartesian(ylim=c(0.8, 1E+3)) + 
    #scale_y_continuous(labels=fancy_scientific)  + 
 
    xlab(paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)")) + 
    ylab(paste0("Percent Change from Baseline in ", PD.TEST.LABEL, " (%)")) + 
    #coord_cartesian(xlim = c(0, 85)) + 
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  
  fig = fig + annotation_logticks(sides ="b") +  # "trbl", for top, right, bottom, and left.
    facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  
  attr(fig, 'title') <- paste0("Hysteresis Plot of Mean Response vs Mean Concentraton by Dose Group", "(", STUDY.NAME, ")")
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["HYSTERESIS_MEAN_PCHG_LOG"]] = fig 
  fig
  
  
  
  ##########################################################################################
  # individual DOUBLE Y-AXIS PLOT, by ARMA
  ##########################################################################################
  
  # data: 
  #---------- 
  tdata = nmdat %>% filter(TEST %in% c(PK.TEST.NAME, PD.TEST.NAME)) %>% 
    mutate(TIME=TIME/7)
  
  # Create a simple secondary axis
  library(ggplot2)
 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$TIME, na.rm=TRUE))) 
 
  # scale PD response
  scaleFactor <- 
    tdata %>% filter(TEST==PK.TEST.NAME)%>%pull(DVOR)%>% max( na.rm=TRUE) / 
    tdata%>%filter(TEST==PD.TEST.NAME)%>%pull(DVOR)%>% max(na.rm=TRUE )  
  
  tdata = tdata %>% mutate(DVOR=ifelse(TEST==PD.TEST.NAME, DVOR*scaleFactor, DVOR)) 
  
  # plot
  #------- 
  fig = ggplot() +  
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    
    geom_point(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=TIME, y=DVOR, group=USUBJID),   col="black") +
    geom_line(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=TIME, y=DVOR, group=USUBJID),   col="black") +
    
    geom_point(data=tdata%>%filter(TEST==PD.TEST.NAME), aes(x=TIME, y=DVOR, group=USUBJID),   col="blue") +
    geom_line(data=tdata%>%filter(TEST==PD.TEST.NAME), aes(x=TIME, y=DVOR, group=USUBJID),   col="blue") +
    
    #xlab("Time (day)") + 
    #ylab("Concentration(SE) of Total REGN3918 (mg/L)") + 
    scale_x_continuous(name="Time (week)", breaks=x$breaks, label=x$labels) +
    scale_y_continuous(name=paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)"), 
                       sec.axis=sec_axis(~., name=paste0(PD.TEST.NAME, " (Unit)"))) + 
    
    theme(
      axis.title.y.left=element_text(color="black"),
      axis.text.y.left=element_text(color="black"),
      axis.title.y.right=element_text(color="blue"),
      axis.text.y.right=element_text(color="blue")
    ) + 
    guides(col=guide_legend(ncol=4, byrow=TRUE)) 
     
  # fig_LN
  fig = fig + facet_wrap(~ARMA, scales="free")
  attr(fig, 'title') <- "Individual Concentration-time Profiles (Linear-scale) of Concentration and PD Response"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["DoubleY_Indiv_Time_Profile_Panel_LN"]] = fig 
  
  fig
  
   
  
  ##########################################################################################
  # Mean DOUBLE Y-AXIS PLOT, by ARMA
  ##########################################################################################
 
  # data: 
  #---------- 
  tdata = nmdat %>% filter(TEST %in% c(PK.TEST.NAME, PD.TEST.NAME))  %>%    # exclude pre-dose samples
    
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    mutate(DVOR = ifelse(TEST==PK.TEST.NAME & DVOR<LLOQ, LLOQ/2, DVOR))  %>% 
    
    mutate(NTIM= NTIM/7) %>% 
    
    # calculate the statistics (Mean, SE, SD)
    calc_stats(id="USUBJID", 
               group_by=c("STUDYID", "TEST","ARMA","ARMAN","NTIM","TIMEPT"), 
               value="DVOR") %>% 
    
    # ARMA = ARMA + (N)
    arrange(ARMAN) %>% group_by(ARMA) %>% 
    mutate(ARMA2 = paste0(ARMA, "(", max(N), ")")) %>% ungroup() %>%   # Note use Max(N)
    mutate(ARMA = ordered(ARMA2, levels=unique(as.character(ARMA2)))) %>%
    
    select(STUDYID, TEST, ARMA, NTIM, TIMEPT, N, Mean, meanMinusSE, meanPlusSE, Mean_SD, SE, Median_Range) %>% 
    arrange(TEST, ARMA, NTIM)
  
  # scale PD response
  scaleFactor <- 
    tdata %>% filter(TEST==PK.TEST.NAME)%>%pull(Mean)%>% max( na.rm=TRUE) / 
    tdata%>%filter(TEST==PD.TEST.NAME)%>%pull(Mean)%>% max(na.rm=TRUE )  
  
  tdata = tdata %>% mutate(Mean=ifelse(TEST==PD.TEST.NAME, Mean*scaleFactor, Mean),
                           meanMinusSE=ifelse(TEST==PD.TEST.NAME, meanMinusSE*scaleFactor, meanMinusSE), 
                           meanPlusSE=ifelse(TEST==PD.TEST.NAME, meanPlusSE*scaleFactor, meanPlusSE)
  )
  
  # plot
  #------- 
  # Create a simple secondary axis
  library(ggplot2)
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE))) 
  
  fig = ggplot() +  
    theme_bw() + base_theme(font.size = as.integer(12)) + 
     
    geom_point(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=NTIM, y=Mean, group=ARMA), col="black") +
    geom_line(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=NTIM, y=Mean, group=ARMA), col="black") +
    geom_errorbar(data=tdata%>%filter(TEST==PK.TEST.NAME), aes(x=NTIM, y=Mean, ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    geom_point(data=tdata%>%filter(TEST==PD.TEST.NAME), aes(x=NTIM, y=Mean, group=ARMA),col="blue") +
    geom_line(data=tdata%>%filter(TEST==PD.TEST.NAME), aes(x=NTIM, y=Mean, group=ARMA), col="blue") +
    geom_errorbar(data=tdata%>%filter(TEST==PD.TEST.NAME), aes(x=NTIM, y=Mean,ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    #xlab("Time (day)") + 
    #ylab("Concentration(SE) of Total REGN3918 (mg/L)") + 
    scale_x_continuous(name="Time (week)", breaks=x$breaks, label=x$labels) +
    scale_y_continuous(name=paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)"), 
                       sec.axis=sec_axis(~., name=paste0(PD.TEST.NAME, " (Unit)"))) + 
    
    theme(
      axis.title.y.left=element_text(color="black"),
      axis.text.y.left=element_text(color="black"),
      axis.title.y.right=element_text(color="blue"),
      axis.text.y.right=element_text(color="blue")
    ) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE)) 
  
  # fig_LN
  fig = fig + facet_wrap(~ARMA, scales="free")
  attr(fig, 'title') <- "Double-Y Plot of Individual Concentration and Percent Change from Baseline in CH50 vs Time by Treatment Group in Analysis Set"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["DoubleY_Mean_Time_Profile_Panel_LN"]] = fig 
  
  fig
  
  
  
  
  ##########################################################################################
  # Response Vs Time by Quantile of Concentration (or exposure)
  ##########################################################################################
  
  # data: 
  #---------- 
  tdata = PKPD %>% filter(ARMA !="Placebo")
   
  tdata = tdata %>% left_join(tdata %>% group_by(TIMEPT, NTIM) %>% 
                                dplyr::summarise_at("PK", funs(min, q25, mean,q50, mean, q75, max)), 
                              by=c("TIMEPT", "NTIM"))
  
  tdata = tdata %>% group_by(TIMEPT, NTIM) %>% 
    mutate(Quantile = ifelse(PK<=q25, "Q1", 
                             ifelse(PK>q25 & PK<=q50, "Q2",
                                    ifelse(PK>q50 & PK<=q75, "Q3", 
                                           ifelse(PK>q75, "Q4", NA  )))))  %>% 
    arrange(NTIM, Quantile)
       
  # stats for PD response
  tdata = tdata %>% mutate(PCHG=as_numeric(PCHG)) %>% 
    group_by(TIMEPT, NTIM, Quantile) %>% 
    dplyr::summarise(Mean = mean(PCHG), 
                     SD = sd(PCHG),
                     SE = SD/sqrt(length(PCHG))
                     ) %>% 
    #dplyr::summarise_at("PCHG", funs(min, q25, mean, q50, mean, q75, max)) %>%  
    arrange(NTIM, Quantile) %>% ungroup()
  
  
  
  
  # ARMA = ARMA + (N) 
  tdata = tdata %>% mutate(ARMA=Quantile)
  
  # plot
  #------- 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE))) 
  
  
  #y=setup_axis(xscale='100_200', xlimits=c(0, max(tdata$meanPlusSE, na.rm=TRUE)))
  
  # no pre-dose samples in plot
  fig = ggplot(tdata%>% mutate(NTIM=NTIM/7), aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +     
    geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width=0.2) + 
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0("Mean(±SE) Percent Change from Baseline in CH50 (%)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))    
  
  #geom_hline(yintercept=c(4.9, 27.8, 0.039), lty="dashed") + 
  #geom_text(y=log10(24 ), x=112, aes(label="27.8 mg/L", hjust=0),   color='black')  
  
  fig
  
  # linear scale
  attr(fig, 'title') <-  paste0("Mean(±SE) Percent Change from Baseline in CH50 (", STUDY.NAME, ")")
  FIGURE_ALL[["MeanPD_TIME_by_Quantile_LN"]] = fig 
  
  # # log scale 
  # fig = fig + scale_y_log10(breaks = 10^(seq(-1,3,by=1)),      #trans_breaks("log10", function(x) 10^x),
  #                           labels = 10^(seq(-1,3,by=1))) +      # trans_format("log10", math_format(10^.x))) +
  #   annotation_logticks(sides ="l")  +  # "trbl", for top, right, bottom, and left.
  #   
  #   geom_hline(yintercept=c(0.078), lty="dashed") + 
  #   geom_text(y=log10(0.092 ), x=7, aes(label="BLQ=0.078 mg/L", hjust=0), size=4, color='black')  
  # 
  # fig
  # attr(fig, 'title') <- paste0("Mean(±SE) Log-scaled Concentration of ", PK.TEST.LABEL, " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  # FIGURE_ALL[["MeanPD_TIME_by_Quantile_LN"]] = fig 
  
  # listing table
  tabl = tdata %>% select(Quantile, TIMEPT, NTIM, Mean, SD, SE) 
  attr(tabl, 'title') <-  paste0("Descriptive Statistics of ", PK.TEST.LABEL, " in Serum ", "(", STUDY.NAME, ")")
  LISTING_ALL[["MeanPD_TIME_by_Quantile_LN"]] = tabl
  
   
  ###########################################
  # print to word and ppt
  ###########################################
  
  if (idebug ==1) { 
    
    # use customized version
    myppt <- pptx(title = "title", template = '~/FYANG/LIB/pptTemplate.pptx')
    mydoc <- docx(template = "~/FYANG/LIB/memoTemplate.docx", empty_template = FALSE)
    
    tt = print2_word_ppt(FIGURE_ALL, TABLE_ALL,  mydoc, myppt) 
    
    writeDoc(tt$mydoc, file = './docs/memo_results.docx')
    
    writeDoc(tt$myppt, file = './docs/ppt_results.pptx')
    
  }
  
  
  
