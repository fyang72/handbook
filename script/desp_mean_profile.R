################################################################################
# Mean time profile
################################################################################

desp_mean_profile <-function(dataset, params=NULL) {
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character  
  
  figure=NULL
  table =NULL
  data = NULL
  
  if (1==1) { 
    #HOME = "~/FYANG/R1979_CD20_CD3/"  
    load(paste0("~/FYANG/R1979_CD20_CD3/", "/KRM/data/nmdat_160mg_320mg.RData"))
    
    dataset = nmdat_upto320mg %>% 
      mutate(DOSEGRP=ARMA, 
             TESTCD=TEST, 
             TESTLABL = "Concentration of REGN1979"
      ) 
  }
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c(
    "STUDYID", "USUBJID", "DOSEGRP", 
    "NTIM", "TIMEPT", 
    "EXTRT", 
    "TESTCD", "TESTLABL", "DVOR", "TIME", "LLOQ")
  missing.column.lst <- key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message <- paste0("missing variable(s) of ", 
                    paste0(missing.column.lst, collapse=", "))
  
  #validate(need(all(key.column.lst %in% colnames(dataset)), message=message) )
  
  #----------------------------------
  # derived information fromd dataset
  #-----------------------------------
  study_name = ifelse(!"STUDYID" %in% colnames(dataset), "STUDYID", 
                      paste0(unique(dataset$STUDYID), collapse=" "))
  
  drug_name = ifelse(!"EXTRT" %in% colnames(dataset), "EXTRT",
                     paste0(unique(dataset$EXTRT), collapse=" "))
  
  test_name = ifelse(!"TESTCD" %in% colnames(dataset), "TESTCD",
                     paste0(unique(dataset$TESTCD), collapse=" "))
  
  test_label = ifelse(!"TESTLABL" %in% colnames(dataset), "TESTLABL",
                      paste0(unique(dataset$TESTLABL), collapse=" "))
  
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
  dosegrp.lst <- unique(tdata$DOSEGRP) %>% sort() ; # print(dosegrp.lst)
  tdata = tdata %>%  mutate(DOSEGRP=ordered(DOSEGRP, levels=dosegrp.lst))
  
  #------------------
  #  calculate stats:
  #------------------ 
  tdata = tdata  %>%    # exclude pre-dose samples
    
    # calculate the statistics (Mean, SE, SD)
    calc_stats(id="USUBJID", 
               group_by=c("STUDYID", "TESTCD","DOSEGRP", "NTIM","TIMEPT"), 
               value="DVOR") %>% 
    
    # DOSEGRP = DOSEGRP + (N)
    arrange(DOSEGRP) %>% group_by(DOSEGRP) %>% 
    mutate(DOSEGRP2 = paste0(DOSEGRP, "(", max(N), ")")) %>% # Note Max(N)
    ungroup() %>%   
    mutate(DOSEGRP2 = ordered(DOSEGRP2, levels=unique(as.character(DOSEGRP2)))) %>%
    
    select(STUDYID, TESTCD, DOSEGRP, DOSEGRP2, NTIM, TIMEPT, 
           N, Mean, meanMinusSE, meanPlusSE, 
           Mean_SD, SE, Median_Range) %>% 
    arrange(TESTCD, DOSEGRP, NTIM)
  
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
               aes(x=xvar, y=yvar, group=DOSEGRP2, col=DOSEGRP2)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point() + geom_line() +   
    geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
    
    scale_color_manual(values=colScheme()) +  
    
    scale_x_continuous(breaks=x$breaks, label=x$labels) +
    #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    
    xlab("Time (week)") +  
    ylab(paste0("Mean(±SE) ", test_label, " (mg/L)")) +   
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4, byrow=TRUE))    
  
  fig
  
  #------------------
  # linear scale
  #------------------
  attr(fig, 'title') <- paste0(
    "Mean(±SE) ", test_label, 
    " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
    drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6                                
  figure[["pk_mean_profile_ln"]] = fig 
  figure[["pk_mean_profile_ln"]]$data =  tdata
  
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
    "Mean(±SE) Log-scaled ", test_label, 
    " in Serum vs Nominal Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", 
    drug_name, " (", study_name, ")")
  
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6                             
  figure[["pk_mean_profile_log"]] = fig 
  figure[["pk_mean_profile_log"]]$data =  tdata
  
  #------------------
  # associated table
  #------------------
  tabl = tdata %>% select(STUDYID, TESTCD, DOSEGRP2, TIMEPT, NTIM, Mean_SD) %>% 
    mutate(DOSEGRP2 = gsub("(", "\n(", DOSEGRP2, fix=TRUE), 
           DOSEGRP2 = ordered(DOSEGRP2, levels=unique(DOSEGRP2))
    ) %>% 
    spread(key=DOSEGRP2, value=Mean_SD) %>% 
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
  
  attr(tabl, 'title') <-  paste0(
    "Descriptive Statistics (±SD) of ", 
    test_label, " in Serum ", "(", study_name, ")")
  
  table[["pk_stats_tab"]] = tabl
  
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = desp_mean_profile(dataset, params=NULL)
  
}
