################################################################################
# Mean time profile
################################################################################

descriptive_time_profile_mean <-function(dataset, params=NULL) {
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character  
  
  figure=NULL
  table =NULL
  data = NULL
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c(
    "STUDYID", "USUBJID", "ARMA", 
    "NTIM", "TIMEPT", 
    "EXTRT", 
    "TESTCD", "TEST", "DVOR", "TIME", "LLOQ")
  missing.column.lst <- key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message <- paste0("missing variable(s) of ", 
                    paste0(missing.column.lst, collapse=", "))
  
  #validate(need(all(key.column.lst %in% colnames(dataset)), message=message) )
  
  #----------------------------------
  # derived information fromd dataset
  #-----------------------------------
  study_name = ifelse(!"STUDYID" %in% colnames(dataset), "STUDYID", 
                      paste0(dataset %>% drop_na(STUDYID) %>% pull(STUDYID) %>% unique(),  collapse=", "))
  
  drug_name = ifelse(!"EXTRT" %in% colnames(dataset), "EXTRT",
                     paste0(dataset %>% drop_na(EXTRT) %>% pull(EXTRT) %>% unique(),  collapse=", "))
  
  test_name = ifelse(!"TESTCD" %in% colnames(dataset), "TESTCD",
                     paste0(dataset %>% drop_na(TESTCD) %>% pull(TESTCD) %>% unique(),  collapse=", "))
  
  test_label = ifelse(!"TEST" %in% colnames(dataset), "TEST",
                      paste0(dataset %>% drop_na(TEST) %>% pull(TEST) %>% unique(),  collapse=", "))
  
  
  #------------------------------           
  # prepare the dataset 
  #------------------------------
  tdata = dataset  %>%   
    mutate(DVOR=as_numeric(DVOR), 
           NTIM=as_numeric(NTIM),
           TIME=as_numeric(TIME), 
           LLOQ = as_numeric(LLOQ)
    ) %>% 
    filter(TIME>=0) %>%  # exclude pre-dose samples
    filter(EVID==0) %>%  # exclude dosing events
    mutate(LLOQ = ifelse(is.na(LLOQ), 0, LLOQ)) %>% 
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) 
    # are set to LLOQ/2 
    mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  
  
  # order the dose group  
  ARMA.lst <- unique(tdata$ARMA) %>% sort() ; # print(ARMA.lst)
  tdata = tdata %>%  mutate(ARMA=ordered(ARMA, levels=ARMA.lst))
  
  #------------------
  #  calculate stats:
  #------------------ 
  tdata = tdata  %>%    # exclude pre-dose samples
    
    # calculate the statistics (Mean, SE, SD)
    calc_stats(id="USUBJID", 
               group_by=c("STUDYID", "TESTCD","ARMA","ARMAN", "NTIM","TIMEPT"), 
               value="DVOR") %>% 
    
    # ARMA = ARMA + (N)
    group_by(ARMA) %>% 
    mutate(ARMA2 = paste0(ARMA, "(", max(N), ")")) %>% # Note Max(N)
    ungroup() %>% arrange(ARMAN) %>% 
    mutate(ARMA2 = ordered(ARMA2, levels=unique(as.character(ARMA2)))) %>%
    
    select(STUDYID, TESTCD, ARMA, ARMA2, NTIM, TIMEPT, 
           N, Mean, meanMinusSE, meanPlusSE, 
           Mean_SD, SE, Median_Range) %>% 
    arrange(TESTCD, ARMA, NTIM)
  
  #------------------
  # plot
  #------------------
  tdata <- tdata %>% mutate(xvar = NTIM/7, 
                            yvar = Mean 
  )
  
  x=setup_scale(myscale='1_2', 
                mylimit=c(0, max(tdata$xvar, na.rm=TRUE))
  )
  
  # no pre-dose samples in plot
  fig = ggplot(tdata, 
               aes(x=xvar, y=yvar, group=ARMA2, col=ARMA2)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point() + geom_line() +   
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    scale_color_manual(values=colScheme()) +  
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0("Mean(SE) ", test_label, " (mg/L)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4, byrow=TRUE))    
  
  fig
  
  #------------------
  # linear scale
  #------------------
  attr(fig, 'title') <- paste0(
    "Mean ", test_label, 
    " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
    drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6                                
  figure[[paste0("mean_profile_ln_", test_name)]] = fig 
  figure[[paste0("mean_profile_ln_", test_name)]]$data =  tdata
  
  #------------------
  # log scale 
  #------------------
  fig = fig + 
    scale_y_log10(breaks = 10^(seq(-3,3,by=1)), #trans_breaks("log10", function(x) 10^x),
                  labels = 10^(seq(-3,3,by=1))) +  # trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides ="l")  +  # "trbl", for top, right, bottom, and left.
    
    geom_hline(yintercept=c(0.078), lty="dashed") + 
    geom_text(y=log10(0.092 ), x=0.1, 
              aes(label="BLQ=0.078 mg/L", hjust=0), size=4, color='black')  
  
  fig
  
  attr(fig, 'title') <- paste0(
    "Mean Log-scaled ", test_label, 
    " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
    drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6                             
  figure[[paste0("mean_profile_log_", test_name)]] = fig 
  figure[[paste0("mean_profile_log_", test_name)]]$data =  tdata
  
  #------------------
  # associated table
  #------------------
  tabl = tdata %>% select(STUDYID, TESTCD, ARMA2, TIMEPT, NTIM, Mean_SD) %>% 
    # mutate(ARMA2 = gsub("(", "(", ARMA2, fix=TRUE), # have an escape here
    #        ARMA2 = ordered(ARMA2, levels=unique(ARMA2))
    # ) %>% 
    spread(key=ARMA2, value=Mean_SD) %>% 
    arrange(TESTCD, NTIM)
  
  # TESTCD
  if (all(tabl$TESTCD==unique(tabl$TESTCD)[1])) {
    tabl$TESTCD=NULL
  }else{
    tabl = tabl %>% mutate(TESTCD = ifelse(duplicated(TESTCD), "", TESTCD))
  }
  
  # STUDYID
  if (all(tabl$STUDYID==unique(tabl$STUDYID)[1])) {
    tabl$STUDYID=NULL
  }else{
    tabl = tabl %>% mutate(STUDYID = ifelse(duplicated(STUDYID), "", STUDYID))
  }
  
  attr(tabl, 'title') <-  paste0("Descriptive Statistics (SD) of ", 
    test_label, " in Serum ", "(", study_name, ")")
  
  table[[paste0("stats_tab_", test_name)]] = tabl
  
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = dataset %>% descriptive_time_profile_mean(params=NULL)
}
