################################################################################
# Individual time profile
################################################################################
 
descriptive_time_profile_indiv <-function(dataset, params=NULL) {

  # initialize variables
  figure=NULL
  table =NULL
  data = NULL
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c("STUDYID", "USUBJID", "ARMA", "NTIM", "TIMEPT", 
                      "TESTCD", "DVOR", "TIME", "LLOQ", "EXTRT")
  missing.column.lst <- 
    key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  
  #message <- paste0("missing variable(s) of ", 
  # paste0(missing.column.lst, sep=", ", collapse=""))
  
  #validate(
  #  need(all(key.column.lst %in% colnames(dataset)), message=message)
  #)
   
  #----------------------------------
  # derived information fromd dataset
  #-----------------------------------
  study_name = ifelse(
    !"STUDYID" %in% colnames(dataset), "STUDYID",
    paste0(dataset %>% drop_na(STUDYID) %>% pull(STUDYID) %>% unique(), collapse=", ")
    )
  
  drug_name = ifelse(
    !"EXTRT" %in% colnames(dataset), "EXTRT",
    paste0(dataset %>% drop_na(EXTRT) %>% pull(EXTRT) %>% unique(), collapse=", ")
    )
  
  test_name = ifelse(
    !"TESTCD" %in% colnames(dataset), "TESTCD",
     paste0(dataset %>% drop_na(TESTCD) %>% pull(TESTCD) %>% unique(), collapse=", ")
     )
  
  test_label = ifelse(
    !"TEST" %in% colnames(dataset), "TEST",
    paste0(dataset %>% drop_na(TEST) %>% pull(TEST) %>% unique(), collapse=", ")
    )
  
  test_unit = ifelse(
    !"DVORU" %in% colnames(dataset), "DVORU",
    paste0(dataset %>% drop_na(DVORU) %>% pull(DVORU) %>% unique(), collapse=" ")
    )
  
  
  #------------------------------           
  # prepare the dataset 
  #------------------------------
  tdata = dataset  %>%  
    mutate(DVOR=as_numeric(DVOR), 
           NTIM=as_numeric(NTIM),
           TIME=as_numeric(TIME), 
           LLOQ=as_numeric(LLOQ), 
           EVID=as.integer(EVID)
           ) %>% 
    filter(TIME>=0) %>%  # exclude pre-dose samples
    filter(EVID==0) %>%  # exclude dosing events
    mutate(LLOQ = ifelse(is.na(LLOQ), 0, LLOQ)) %>% 
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  
  
  # order the dose group  
  ARMA.lst <- unique(tdata$ARMA)
  tdata = tdata %>%  mutate(ARMA=ordered(ARMA, levels=ARMA.lst))
   
  #------------------
  # plot
  #------------------
  tdata <- tdata %>% mutate(xvar = NTIM/7, yvar = DVOR)
  x=setup_scale(myscale='1_2',mylimit=c(0, max(tdata$xvar, na.rm=TRUE)))
  
  fig = ggplot(tdata, aes(x=xvar, y=yvar, group=USUBJID, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point() + geom_line() +   
  
    scale_color_manual(values=colScheme()) +  
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0(test_label, "(", test_unit, ")")) +  
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))    
   
  #------------------
  # linear scale
  #------------------
  attr(fig, 'title') <-  
    paste0("Individual ", 
            test_label, 
            " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
            drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6   
  figure[[paste0("indiv_profile_ln_", test_name)]] = fig 
  figure[[paste0("indiv_profile_ln_", test_name)]]$data =  tdata
  
  #------------------
  # log scale 
  #------------------
  loc_x <- 0  # location of line label 
  BLQ = c(0.078)   #c(0.078, 65)
  label_text = paste0(BLQ, " mg/L" )
  
  fig = fig + scale_y_log10(breaks = 10^(seq(-3,3,by=1)),      #trans_breaks("log10", function(x) 10^x),
                            labels = 10^(seq(-3,3,by=1))) +      # trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides ="l")  +  # "trbl", for top, right, bottom, and left.
    
    geom_hline(yintercept=BLQ, lty="dashed") + 
    geom_text(data=data.frame(x=loc_x, y=BLQ), 
              aes(x, y, label=label_text,  hjust=0, vjust=-1), 
              size=3,  inherit.aes = FALSE,  color='black')
  
  fig
  attr(fig, 'title') <- 
    paste0("Individual Log-scaled ", 
           test_label, " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
           drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6   
  figure[[paste0("indiv_profile_log_", test_name)]] = fig 
  figure[[paste0("indiv_profile_log_", test_name)]]$data =  tdata
   
  return(list(data=data, figure=figure, table=table))  # do keep it as this way, 07-28-2019
  }
  
#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(
    filtered_dataset() %>% descriptive_time_profile_indiv(params=NULL)
  )
  
}
