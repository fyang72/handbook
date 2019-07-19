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
  tdata = nmdat %>% filter(TEST %in% c(PK.TEST.NAME, TARGET.TEST.NAME)) %>%   filter(!is.na(DVOR),   !is.na(TIMEPT))  %>% 
    select(USUBJID, ARMA, TIMEPT, NTIM, TEST, DVOR) #%>% 
    #mutate(DVOR = ifelse(TEST==TARGET.TEST.NAME, PCHG, DVOR))   
  
  tdata = tdata   %>%  spread(TEST, DVOR)
  
  tdata = tdata %>% mutate(ARMA = ordered(ARMA, levels = levels(unique(tdata$ARMA)) )) %>% 
    #rename(PCHG = CH50H) %>% 
    mutate(C5 = as_numeric(C5), 
           REGN3918 = as_numeric(REGN3918))  %>% 
    mutate(REGN3918 = ifelse(REGN3918==0|is.na(REGN3918), 0.039, REGN3918))
  
  PKPD = tdata %>% mutate(NTIM=as_numeric(NTIM), 
                          PK = as_numeric(REGN3918),
                          PD = as_numeric(C5)
                          ) %>% arrange(ARMA, USUBJID, NTIM)
  
  PKPD = PKPD %>% 
    mutate(USUBJID = gsub("R3918-HV-1659-", "", USUBJID, fix=TRUE)) %>% 
    mutate(USUBJID = ordered(USUBJID, levels=unique(USUBJID)))
  
  ##########################################################################################
  # PKPD Sigmoid Scatter plot [ use percent change from baseline]
  ##########################################################################################
  fig = empty_canvas(label="PK-Target-Relationship"); 
  attr(fig, 'title') <- ""
  FIGURE_ALL[["canvas_PK_Target_Relationship"]] = fig  
  
  
  
  tdata = PKPD %>% group_by(ARMA, USUBJID) %>% 
    #filter(ARMA == "400 mg SC QW") %>% 
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
    ylab(paste0("Concentration of ", TARGET.TEST.LABEL, " (mg/L)")) + 
    #coord_cartesian(xlim = c(0, 85)) + 
    
    theme_bw() +   
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  
  fig = fig + annotation_logticks(sides ="b") +  # "trbl", for top, right, bottom, and left.
    theme(panel.grid.minor = element_blank())
  fig
  
  fig0 = fig
  
  
  ############################
  
  
  tdata = PKPD %>% group_by(ARMA, USUBJID) %>% 
    filter(ARMA == "400 mg SC QW") %>% 
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
    ylab(paste0("Concentration of ", TARGET.TEST.LABEL, " (mg/L)")) + 
    #coord_cartesian(xlim = c(0, 85)) + 
    
    theme_bw() +   
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  
  fig = fig + annotation_logticks(sides ="b") +  # "trbl", for top, right, bottom, and left.
    theme(panel.grid.minor = element_blank())
  fig
  
  fig_Coh5 = fig
  
  
  
  ############################
  
  
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
  
  fig = fig0
  attr(fig, 'title') <-  "Sigmoid PK-PD (Percent Change from Baseline) Relationship Regardless of Time Points and Dose Groups [Semi-Log Scale]"
  
  FIGURE_ALL[["SIGMOID_Target_PK"]] = fig
  
  fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <- "Sigmoid PK-PD (Percent Change from Baseline) Relationship Regardless of Time Points and Dose Groups [Semi-Log Scale]"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["SIGMOID_Target_PK_PANEL"]] = fig 
  
  
  
  
  #-------------------------------------------------------------
  # addon-2: individual hysteresis plot, if N is small 
  #-------------------------------------------------------------
  # BY ARMA
  #-------------------------------
  fig = fig0 + facet_wrap(~ARMA) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig
    
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["HYSTERESIS_INDIV_Target_PK_BY_ARMA"]] = fig 
  
  
  
  fig = fig_Coh5 + facet_wrap(~ARMA) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig
  
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["HYSTERESIS_INDIV_Target_PK_BY_ARMA_Coh5"]] = fig 
  
  
  
  
  # by USUBJID
  #-------------------------------
  fig = fig0 + facet_wrap(~USUBJID) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig 
  
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 12
  attr(fig, 'height') <- 9
  FIGURE_ALL[["HYSTERESIS_INDIV_Target_PK_BY_USUBJID"]] = fig 
  
  #
  fig = fig_Coh5 + facet_wrap(~USUBJID) + #ggplot2::theme(legend.position = "none") + #, scales="free")
    geom_segment(aes(xend = xend,yend= yend),
                 arrow=arrow(length=unit(0.2,"cm")))
  fig 
  
  attr(fig, 'title') <- "Hysteresis Plot of Percent Change from Baseline (%) vs. Concentration (mg/L) by USUBJID"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["HYSTERESIS_INDIV_Target_PK_USUBJID_Coh5"]] = fig 
  
  
  
  
  
  
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
    ylab(paste0("Concentration of ", TARGET.TEST.LABEL, " (mg/L)")) + 
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
  FIGURE_ALL[["HYSTERESIS_Mean_Target_PK"]] = fig 
  fig
  
  
   
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
  
  
  
