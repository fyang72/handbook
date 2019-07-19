  #----------------------------------------------------------------------------
  # Version 0.1   Created on 11/08/2018, Feng Yang
  # 
  #----------------------------------------------------------------------------
  # Input
  # -----------------
  # 1) nmdat:  meta data file 
  #
  #
  # Output Figures: 
  #------------------
  # PK 
  # 1) Mean PK profile (log-scale and linear scale, by panel or not)
  # 2) Individual PK profile (log-scale and linear scale, by panel or not)
  # 
  # Target
  # 3) Mean Target profile (log-scale and linear scale, by panel or not)
  # 4) Individual Target profile (log-scale and linear scale, by panel or not)
  
  # PD Response
  # 5) Mean response (raw) profile (log-scale and linear scale, by panel or not)
  # 6) Individual response (raw) profile (log-scale and linear scale, by panel or not) 
  # 7) Mean response (PCHG) profile (log-scale and linear scale, by panel or not)
  # 8) Individual response (PCHG) profile (log-scale and linear scale, by panel or not) 
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
                  col_type=cols(.default=col_character())) %>%   # read as character as defualt
    mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE), 
           TIME=as_numeric(TIME), 
           DVOR = as_numeric(DVOR) 
    )
  
  
  arma.lst <- c("Placebo",  "1 mg/kg IV" ,  "3 mg/kg IV",    "10 mg/kg IV" ,   "30 mg/kg IV" , 
                "600 mg SC" ,  "300 mg SC" ,  
                "400 mg SC QW" )
  
  
  # make ARMA as a ordered factor
  nmdat = nmdat %>% mutate(ARMA = ordered(ARMA, levels=arma.lst), 
                         ARMAN = as.integer(ARMA), 
                         DVOR = as_numeric(DVOR), 
                         TIME = as_numeric(TIME), 
                         NTIM = as_numeric(NTIM),
                         LLOQ = as_numeric(LLOQ)
                         )   
  
  # exclude all dose events
  nmdat = nmdat %>% filter(is.na(as_numeric(EXTDOSE)))
  
  # Do we want to exclude all pre-dose sampels, as the default?
  nmdat = nmdat %>% filter(as_numeric(TIME)>=0)   
    

  ##########################################################################################
  # empty_canvas_std_visualization
  ##########################################################################################
   
  fig = empty_canvas(label="PK Data Visualization"); 
  attr(fig, 'title') <- ""
  FIGURE_ALL[["canvas_std_PK_visualization"]] = fig  
  
   
  ##########################################################################################
  # Mean PK time profile
  ##########################################################################################
  tdata = nmdat   #%>% filter(is.na(as_numeric(EXTDOSE)))  # remove all dosing events
   
  # data: 
  #---------- 
  tdata = tdata %>% filter(TEST==PK.TEST.NAME)  %>%    # exclude pre-dose samples
    
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  %>% 
    
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
  
  
  # plot
  #------- 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE)))
  #y=setup_axis(xscale='100_200', xlimits=c(0, max(tdata$meanPlusSE, na.rm=TRUE)))
　　
  # no pre-dose samples in plot
  fig = ggplot(tdata%>%mutate(NTIM=NTIM/7), aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +   
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    scale_color_manual(values=colScheme()) +  # values = c("black", "blue", "green")) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0("Concentration(SE) of ", PK.TEST.LABEL, " (mg/L)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))    
    
    #geom_hline(yintercept=c(4.9, 27.8, 0.039), lty="dashed") + 
    #geom_text(y=log10(24 ), x=112, aes(label="27.8 mg/L", hjust=0),   color='black')  
  
  fig
   
  # linear scale
  attr(fig, 'title') <-  paste0("Mean(±SE) Concentration of ", PK.TEST.LABEL, " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["PK_MEAN_LN"]] = fig 
  
  # log scale 
  fig = fig + scale_y_log10(breaks = 10^(seq(-1,3,by=1)),      #trans_breaks("log10", function(x) 10^x),
                            labels = 10^(seq(-1,3,by=1))) +      # trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides ="l")  +  # "trbl", for top, right, bottom, and left.
    
        geom_hline(yintercept=c(0.078), lty="dashed") + 
        geom_text(y=log10(0.092 ), x=0.1, aes(label="BLQ=0.078 mg/L", hjust=0), size=4, color='black')  
     
  fig
  attr(fig, 'title') <- paste0("Mean(±SE) Log-scaled Concentration of ", PK.TEST.LABEL, " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["PK_MEAN_LOG"]] = fig 
  
  # listing table
  tabl = tdata %>% select(TEST, ARMA, TIMEPT, NTIM, Mean_SD, SE) 
  attr(tabl, 'title') <-  paste0("Descriptive Statistics of ", PK.TEST.LABEL, " in Serum ", "(", STUDY.NAME, ")")
  LISTING_ALL[["PK_MEAN"]] = tabl
    
  
  ##########################################################################################
  # individual PK plot
  ##########################################################################################
  
  # data: 
  #---------- 
  tdata = nmdat %>% filter(TEST==PK.TEST.NAME, TIME>=0) %>% 
    mutate(TIME=as_numeric(TIME), 
           DVOR = ifelse(as_numeric(DVOR)<as_numeric(LLOQ), as_numeric(LLOQ)/2, as_numeric(DVOR)))
         
  # plot: 
  #---------- 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$TIME, na.rm=TRUE)))
  
  fig = ggplot(tdata %>% mutate(TIME=TIME/7), aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +   
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_x_continuous(trans=‘log2’), scale_y_continuous(trans=‘log2’) : another allowed value for the argument trans is ‘log10’
    
    scale_y_log10(breaks = 10^(seq(-1,3,by=1)), #trans_breaks("log10", function(x) 10^x),
                  labels = 10^(seq(-1,3,by=1))) + # trans_format("log10", math_format(10^.x))) +
    
    #coord_trans( y="log10") +   # possible values for x and y are “log2”, “log10”, “sqrt”, …

    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") + 
    ylab(paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)")) +   
      
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE)) 
  
  # individual log scale 
  fig = fig + annotation_logticks(sides ="l")  +   # "trbl", for top, right, bottom, and left.
    geom_hline(yintercept=c(0.078), lty="dashed") + 
    geom_text(y=log10(0.1 ), x=0.1, aes(label="BLQ=0.078 mg/L", hjust=0), size=3,    color='black')  
  
  attr(fig, 'title') <- paste0("Individual Log-scaled Concentration of ", PK.TEST.NAME, " in Serum vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["PK_INDIV_LOG"]] = fig
  
  # individual log scale by panel
  fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")  
  attr(fig, 'title') <- paste0("Individual Log-scaled Concentration of ", PK.TEST.NAME, " in Serum vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["PK_INDIV_LOG_PANEL"]] = fig 
  fig
  
  
  
  ##########################################################################################
  # Mean+SE TARGET time profile
  ##########################################################################################
   
  fig = empty_canvas(label="TARGET Data Visualization"); 
  attr(fig, 'title') <- ""
  FIGURE_ALL[["canvas_std_TARGET_visualization"]] = fig  
  
  # data: 
  #---------- 
  tdata = nmdat   #%>% filter(is.na(as_numeric(EXTDOSE)))  # remove all dosing events
  
  tdata = tdata %>% filter(TEST==TARGET.TEST.NAME)  %>%    # exclude pre-dose samples
    
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  %>% 
    
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
  
  
  # plot
  #------- 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE)))
  #y=setup_axis(xscale='100_200', xlimits=c(0, max(tdata$meanPlusSE, na.rm=TRUE)))
  
  # no pre-dose samples in plot
  fig = ggplot(tdata%>% mutate(NTIM=NTIM/7), aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +      
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0("Concentration(SE) of ", TARGET.TEST.LABEL, " (mg/L)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))    
  
  #geom_hline(yintercept=c(4.9, 27.8, 0.039), lty="dashed") + 
  #geom_text(y=log10(24 ), x=112, aes(label="27.8 mg/L", hjust=0),   color='black')  
  
  fig
  
  # linear scale
  attr(fig, 'title') <- paste0("Mean (±SE) Concentration of ", TARGET.TEST.LABEL, " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["TARGET_MEAN_LN"]] = fig 
   
  # log scale 
  # Typically, no log-scale needed for target
  
  # listing table
  tabl = tdata %>% select(TEST, ARMA, TIMEPT, NTIM, Mean_SD, SE) 
  attr(tabl, 'title') <-  paste0("Descriptive Statistics of ", TARGET.TEST.LABEL, " in Serum ", "(", STUDY.NAME, ")")
  LISTING_ALL[["TARGET_MEAN"]] = tabl
   
  
  ##########################################################################################
  # individual TARGET time profile
  ##########################################################################################
   
  # data: 
  #---------- 
  tdata = nmdat %>% filter(TEST==TARGET.TEST.NAME, TIME>=0) %>% 
    mutate(TIME=as_numeric(TIME), 
           DVOR = ifelse(as_numeric(DVOR)<as_numeric(LLOQ), as_numeric(LLOQ)/2, as_numeric(DVOR)))
  
  # plot: 
  #---------- 
 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$TIME, na.rm=TRUE)))
  
  fig = ggplot(tdata %>%mutate(TIME=TIME/7), aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +    
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_x_continuous(trans=‘log2’), scale_y_continuous(trans=‘log2’) : another allowed value for the argument trans is ‘log10’
    
    #scale_y_log10(breaks = 10^(seq(-1,3,by=1)), #trans_breaks("log10", function(x) 10^x),
    #              labels = 10^(seq(-1,3,by=1))) + # trans_format("log10", math_format(10^.x))) +
    
    #coord_trans( y="log10") +   # possible values for x and y are “log2”, “log10”, “sqrt”, …
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") + 
    ylab(paste0("Concentration of ", TARGET.TEST.LABEL, " (mg/L)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE)) 
  
  # attr(fig, 'title') <- paste0("Individual Concentration-time Profiles of ", TARGET.TEST.LABEL, " By Dose Group")
  # FIGURE_ALL[["ARGET_INDIV"]] = fig 
  
  fig = fig + facet_wrap(~ARMA)+ ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <-  paste0("Individual Concentration of ", TARGET.TEST.LABEL, " in Serum vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["TARGET_INDIV_LN_PANEL"]] = fig 
  fig
  
  
  
  
  ##########################################################################################
  # mean PD  profiles
  ##########################################################################################
  fig = empty_canvas(label=paste0(PD.TEST.LABEL, " Data Visualization")); 
  attr(fig, 'title') <- ""
  FIGURE_ALL[["canvas_PD_Visualization"]] = fig  
   
  # data: 
  #---------- 
  tdata = nmdat   #%>% filter(is.na(as_numeric(EXTDOSE)))  # remove all dosing events
  tdata = tdata %>% filter(TEST==PD.TEST.NAME)  %>%    # exclude pre-dose samples
    
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    #mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  %>% 
    
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
  
  
  # plot
  #------- 
   
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE))) 
  y=setup_scale(myscale='25_50', mylimit=c(0, max(tdata$meanPlusSE, na.rm=TRUE)))
  
  fig = ggplot(tdata%>%mutate(NTIM=NTIM/7) , aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) +
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +   
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2)  + 
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
     
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    scale_y_continuous(breaks=y$breaks, label=y$labels) +  
  
    xlab("Time (week)") + 
    ylab(paste0("Mean(SE) of ", PD.TEST.LABEL, " (U/mL)")) + 
    
    #coord_cartesian(xlim = c(0, 85)) + 
    theme_bw() +  base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))  
     
  
  fig
   
  attr(fig, 'title') <- paste0("Mean (±SE) ", PD.TEST.LABEL, " vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["PD_MEAN_LN"]] = fig  
  
    
  tabl = tdata %>% select(TEST, ARMA, TIMEPT, NTIM, Mean_SD, SE) 
  attr(tabl, 'title') <-  paste0("Descriptive Statistics of ", PD.TEST.LABEL,  "(", STUDY.NAME, ")")
  LISTING_ALL[["PD_MEAN"]] = tabl
  
  
  ##########################################################################################
  # Indiv PD  profiles
  ##########################################################################################
  # data: 
  #---------- 
  tdata = nmdat %>% filter(TEST==PD.TEST.NAME, TIME>=0)  %>% 
        mutate(TIME=as_numeric(TIME), 
               DVOR = as_numeric(DVOR))
  
  # plot: 
  #----------  
  
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE))) 
  y=setup_scale(myscale='50_100', mylimit= c(0, max(tdata$DVOR, na.rm=TRUE)))
 
  fig = ggplot(tdata %>%mutate(TIME=TIME/7), aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() +  

    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    scale_y_continuous(breaks=y$breaks, label=y$labels) +  
  
    xlab("Time (week)") + 
    ylab(paste0(PD.TEST.LABEL, " (U/mL)")) + 
    
    #coord_cartesian(xlim = c(0, 85)) + 
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))  
     
  fig
  
  # attr(fig, 'title') <- paste0("Individual Time Profiles of ", PD.TEST.LABEL, " By Dose Group")
  # FIGURE_ALL[["PD_INDIV"]] = fig  
  
  
  fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <- paste0("Inividual ", PD.TEST.NAME, " vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["PD_INDIV_LN_PANEL"]] = fig 
  fig
  
    
  
  ##########################################################################################
  # mean PCHG OF PD profiles
  ##########################################################################################
    
  # data: 
  #---------- 
  tdata = nmdat   #%>% filter(is.na(as_numeric(EXTDOSE)))  # remove all dosing events  
  tdata = tdata %>% filter(TEST==PD.TEST.NAME)  %>%    # exclude pre-dose samples
    
    # use PCHG as DVOR
    mutate(DVOR = as_numeric(PCHG),   # the key
           NTIM = as_numeric(NTIM))  %>% 
    
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
  
  # plot
  #-------  
 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$NTIM, na.rm=TRUE))) 
  y=setup_scale(myscale='50_100', mylimit=c(-100, 50))
  
  
  
  fig = ggplot(tdata %>%mutate(NTIM=NTIM/7), aes(x=NTIM, y=Mean, group=ARMA, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() + 
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2)  + 
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
      
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    scale_y_continuous(breaks=y$breaks, label=y$labels) +
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") + 
    ylab(paste0("Percent Change(SE) from Baseline in ", PD.TEST.LABEL, " (%)")) + 

    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  fig
  
   
  attr(fig, 'title') <-paste0("Mean (±SE) ", PD.PCHG.LABEL, " vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  FIGURE_ALL[["PD_PCHG_MEAN_LN"]] = fig 
   
  tabl = tdata %>% select(STUDYID, TEST, ARMA, TIMEPT, NTIM, Mean_SD, SE) 
  attr(tabl, 'title') <-  paste0("Descriptive Statistics of ", PD.PCHG.LABEL,  "(", STUDY.NAME, ")")
  LISTING_ALL[["PD_PCHG_MEAN"]] = tabl
  
  
  ##########################################################################################
  # individual PCHG PD plot 
  ##########################################################################################
  
  # data: 
  #---------- 
  tdata = nmdat   #%>% filter(is.na(as_numeric(EXTDOSE)))  # remove all dosing events  
  tdata = tdata %>% filter(TEST==PD.TEST.NAME)  %>%    # exclude pre-dose samples
    
    # use PCHG as DVOR
    mutate(DVOR = as_numeric(PCHG),   # the key
           NTIM = as_numeric(NTIM)) 
  
  # plot
  #-------
 
  x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$TIME, na.rm=TRUE))) 
  y=setup_scale(myscale='25_50', mylimit=range(tdata$DVOR))
 
  fig = ggplot(tdata %>% mutate(TIME=TIME/7), aes(x=TIME, y=DVOR, group=USUBJID, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  geom_line() + 
    #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2)  + 
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
     
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") + 
    ylab(paste0("Percent Change from Baseline in ", PD.TEST.LABEL, "(%)")) + 
     
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
   
  fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")
  attr(fig, 'title') <- paste0("Inividual ", "Percent Change from Baseline in CH50 (%)", " vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["PD_PCHG_INDIV_LN_PANEL"]] = fig
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
  
    
  


