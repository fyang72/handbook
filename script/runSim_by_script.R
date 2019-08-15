##########################################################################################
# run_simulation_by_script
##########################################################################################

postProcess_simData <-function(simData, params=NULL) {
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character 
  
  figure=NULL
  table =NULL
  data = NULL

  
  data[["simData"]] = simData
   
  #---------------------------------------- 
  # priminary PK plot     
  # ---------------------------------------
  tdata = simData  #%>% 
    #mutate(ordered(ARMA, levels=unique(ARMA))
  
  x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$TIME/7, na.rm=TRUE)))
  tdata = tdata %>% 
    mutate(xvar = TIME/7, 
           yvar = IPRED
    )
  
  figLn = ggplot(data=tdata, #%>%mutate(USUBJID=paste0(ARMA,USUBJID)), 
                 aes(x=xvar, y=yvar, group=ID, col=ARMA)) + 
    geom_line() + 

    xlab("Time (Week)") + 
    ylab("Predicted Concentration (mg/L)") + 
    
    #coord_cartesian(ylim = c(1E-2, 300)) + 
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    
    scale_color_manual(values = colScheme()) +  #c("black", "blue",   "black",  "red")) + #"cyan",
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_x_continuous(breaks=seq(0, 112, by=7), label=seq(0, 112, by=7)) +
    
    guides(col=guide_legend(ncol=3, byrow=TRUE))   + 
    
    ggplot2::theme(panel.grid.minor = element_line(colour = "gray98",size=0.75))   
  
  #-----------------------
  # fig1: Linear profile
  #-----------------------
  hline = 0.078
  hline_location <- 2  # location of line label 
  fig = figLn + 
    #geom_hline(yintercept=c(BLOQ), lty="dashed") + 
    #geom_text(y=BLOQ*100, x=loc_x, aes(label=paste0("BLQ=", BLOQ, " mg/L"), hjust=0), size=3, color='black')   

    geom_hline(yintercept = hline, lty="dashed") + 
    annotate("text", hline_location, hline, vjust = -1, label=paste0("BLQ=", hline, " mg/L")) 
             
  attr(fig, 'title') <-   "Predicted Individual Concentration-time Profiles for Proposed Dosing Regimens"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  figure[["SIM_INDIV_PK_LN"]]  = fig
  
  #-----------------------
  # fig2: semi-log profile
  #-----------------------  
  hline = 0.078
  hline_location <- 2  # location of line label 
  fig = figLn +   
    scale_y_log10(breaks = 10^(seq(-3,3,by=1)),      #trans_breaks("log10", function(x) 10^x),
                  labels = 10^(seq(-3,3,by=1))) +      # trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides ="l")  +   # "trbl", for top, right, bottom, and left.
    
    # geom_hline(yintercept=c(BLOQ), lty="dashed") + 
    # geom_text(y=log10(BLOQ*1.25), x=loc_x, aes(label=paste0("BLQ=", BLOQ, " mg/L"), hjust=0), size=3, color='black')   
    # 
    geom_hline(yintercept = hline, lty="dashed") + 
    annotate("text", hline_location, hline, vjust = -1, label=paste0("BLQ=", hline, " mg/L"))
  
  attr(fig, 'title') <-   "Predicted Semi-log Concentration-time Profiles for Proposed Dosing Regimens"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  figure[["SIM_INDIV_PK_LOG"]]  = fig 

  # ---------------------------------------
  # fig3/4: add DVOR 
  # ---------------------------------------
  if (!is.null(dataset)) {
    # linear scale
    fig <- figure[["SIM_INDIV_PK_LN"]] + 
      geom_point(
        data=dataset , 
        aes(x=as_numeric(TIME)/7, 
            y=as_numeric(DVOR)),  
        inherit.aes=FALSE) +  
      facet_wrap(~USUBJID)
    
    attr(fig, 'title') <-   "Predicted Concentration-time Profiles Overlaid with Observed for Proposed Dosing Regimens"
    attr(fig, 'width') <- 10
    attr(fig, 'height') <- 6
    figure[["SIM_INDIV_PK_LN_DATA"]]  = fig
    
    # semi-log
    fig <- figure[["SIM_INDIV_PK_LOG"]] + 
      geom_point(
        data=dataset , 
        aes(x=as_numeric(TIME)/7, 
            y=as_numeric(DVOR)),  
        inherit.aes=FALSE) +  
      facet_wrap(~USUBJID)
    
    attr(fig, 'title') <-   "Predicted Semi-log Concentration-time Profiles Overlaid with Observed for Proposed Dosing Regimens"
    attr(fig, 'width') <- 10
    attr(fig, 'height') <- 6
    figure[["SIM_INDIV_PK_LOG_DATA"]]  = fig
  }
  
  
  
  #####################################################
  # table1/2: simdata_summary_exposure  
  ##################################################### 
  #tdata = simData # USUBJID=paste0(ARMA, USUBJID))
  
  # the whole profile
  #------------------------------------------
  tt = simdata_summary_exposure(simData)$table
  tabl = tt[["summary_exposure_long"]]
  attr(tabl, 'title') <-   "Summary of Exposure by Dose Group for the Overall Concentration-time Profiles "
  table[["summary_exposure_long_upto_infinity"]] = tabl
  
  #tabl = tt[["summary_exposure_wide"]]
  #attr(tabl, 'title') <-   "Summary of Exposure by Dose Group for the Overall Concentration-time Profiles "
  #table[["summary_exposure_wide_upto_infinity"]] = tabl
  
  # filter out the last dosing interval
  #------------------------------------------
  group_lst = c("STUDYID", "ARMA", "TEST", "ID") #TEST:[PK,PD]
  tdata = simData  %>%  
    group_by_(.dots =group_lst)  %>%  
    top_n(n=1, wt=EXSEQ) %>% filter(TAD<=II)    # do it outside
  
  if (nrow(tdata)>0) {
    tt = simdata_summary_exposure(tdata)$table
    tabl = tt[["summary_exposure_long"]]
    attr(tabl, 'title') <-   "Summary of Exposure by Dose Group for the Last Dose Interval "
    table[["summary_exposure_long_last_dose_interval"]] = tabl
    
    #tabl = tt[["summary_exposure_wide"]]
    #attr(tabl, 'title') <-   "Summary of Exposure by Dose Group for the Last Dose Interval"
    #table[["summary_exposure_wide_last_dose_interval"]] = tabl
  }
  
  return(list(figure=figure, table=table, data=data))
}

#################################################################
# final output
#################################################################
if (ihandbook) {

  which_mode = 1
  
  # runSim_by_dosing_regimen    
  # ---------------------------------------  
  if (which_mode==1) { 
    simData = suppressWarnings(runSim_by_dosing_regimen(
      cppModel = params()[["cppModel"]],    # model file
      adsl = params()[["adsl"]],   # population 
      adex = params()[["adex"]],   # dose regimen
      simulation_delta = params()[["simulation_delta"]], #simulation_delta,  # integration step                  
      tgrid = NULL,     # extra timepoint (other than delta)
      infusion_hrs_lst =  params()[["infusion_hrs_lst"]], #infusion_hrs_lst,  # hour,  infusion hours
      followup_period = params()[["followup_period"]], #followup_period,   # how long of the followup period after treatment
      seed=params()[["seed"]]
    )) 
    
    output = suppressWarnings(postProcess_simData(simData, params))
  }
  
  # use a nonmem dataset
  # ---------------------------------------   
  if (which_mode==2) {
    if (!is.null(params()[["adsl"]])) {
      col_lst <- c("USUBJID", setdiff(colnames(params()[["adsl"]]), colnames(dataset)))
      nmdat = dataset %>%
        left_join(params()[["adsl"]] %>%  
                    distinct(USUBJID, .keep_all=TRUE) %>% 
                    select(one_of(col_lst)),   
                  by=c("USUBJID")
        )
    }
    
    # 
    simData <- suppressWarnings(runSim_by_nmdat(
      params()[["cppModel"]],    # model file
      nmdat, 
      simulation_delta = params()[["simulation_delta"]], #simulation_delta,  # integration step                  
      tgrid = NULL,     # extra timepoint (other than delta)
      infusion_hrs_lst =  params()[["infusion_hrs_lst"]], #infusion_hrs_lst,  # hour,  infusion hours
      treat_end = max(dataset$TIME%>%as_numeric(), na.rm=TRUE), 
      sim_end = max(dataset$TIME%>%as_numeric(), na.rm=TRUE), 
      seed=params()[["seed"]]
    )) 
    
    output = suppressWarnings(postProcess_simData(simData, params))
  }
  
}