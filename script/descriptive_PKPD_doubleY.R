##########################################################################################
# Mean time profile
##########################################################################################

descriptive_PKPD_doubleY <-function(dataset, params=NULL) {
  #
     # dataset=read_csv("./data/nmdatPKPD.csv") %>%   #filter(TESTCAT=="RESP1")
     #          mutate(TESTCAT  = gsub("RESP1", "RESP", TESTCAT , fixed=TRUE)) %>% 
     #          filter(TESTCAT %in% c("PK", "RESP"))
     # 
  figure=NULL
  table =NULL
  data = NULL
  message=NULL
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c("STUDYID", "USUBJID", "ARMA", "NTIM", "TIMEPT", "TESTCD", "DVOR", "TIME", "LLOQ", "EXTRT", "EVID")
  missing.column.lst <- key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message <- paste0("missing variable(s) of ", paste0(missing.column.lst, sep=", ", collapse=""))
  
  #validate(need(all(key.column.lst %in% colnames(dataset)), message=message))
  
  #----------------------------------
  # derived information fromd dataset
  #-----------------------------------
  study_name = ifelse(!"STUDYID" %in% colnames(dataset), "STUDYID", 
                      paste0(unique(dataset$STUDYID), sep=" ", collapse=""))
  
  drug_name = ifelse(!"EXTRT" %in% colnames(dataset), "EXTRT",
                     paste0(unique(dataset$EXTRT), sep=" ", collapse=""))
  
  pk_label = dataset %>% filter(TESTCAT=="PK") %>% select(TEST) %>% unique()
  pk_label = pk_label[1]
  
  pd_label = dataset %>% filter(TESTCAT=="PD") %>% select(TEST) %>% unique()
  pd_label = pd_label[1]
  
  #------------------------------           
  # prepare the dataset 
  #------------------------------
  tdata = dataset  %>%  
    mutate(DVOR=as_numeric(DVOR), 
           NTIM=as_numeric(NTIM),
           TIME=as_numeric(TIME), 
           LLOQ = as_numeric(LLOQ), 
           EVID = as.integer(EVID)
    ) %>%  
    filter(TIME>=0, EVID==0) %>%  # exclude pre-dose samples
    mutate(LLOQ = ifelse(is.na(LLOQ), 0, LLOQ)) %>% 
    #Concentrations below the lower limit of quantification (LLOQ = 0.078 mg/L) are set to LLOQ/2 
    mutate(DVOR = ifelse(DVOR<LLOQ, LLOQ/2, DVOR))  
  
  # order the dose group  
  ARMA.lst <- unique(tdata$ARMA) 
  tdata = tdata %>%  mutate(ARMA=ordered(ARMA, levels=ARMA.lst))
   
  # Spread based on TESTCAT
  tdata = tdata %>%  select(STUDYID, USUBJID, ARMA, TIMEPT, NTIM,  DVOR, TESTCAT) %>%
    #unite(temp, STUDYID, USUBJID, ARMA, TIMEPT, NTIM) %>% 
    #distinct(temp, TESTCAT,  .keep_all=TRUE) %>% 
    distinct(STUDYID, USUBJID, ARMA, TIMEPT, NTIM, TESTCAT,  .keep_all=TRUE) %>% 
    spread(TESTCAT, DVOR)  %>% 
    arrange(ARMA, USUBJID, NTIM)  
  
  #------------------
  # plot
  #------------------
  tdata <- tdata%>% group_by(ARMA, USUBJID) %>% 
    mutate(xvar = PK, 
           yvar = PD 
    ) %>% 
    
    mutate(xend = lead(xvar,n=1),  # c(tail(Mean_PK, n=-1), NA)
           yend = lead(yvar,n=1)   # c(tail(Mean_PD, n=-1), NA)
           
    )%>% fill(xend = xend, 
              yend = yend)
  
 
  fig = ggplot(tdata , aes(x=xvar, y=yvar, col=ARMA)) + 
    #ggtitle("Concentration Time Profile") + 
    
    geom_point(aes(shape=ARMA, size=ARMA)) +  #geom_line() +    
    
    scale_color_manual(values=colScheme()) + 
    scale_shape_manual(values=shapeScheme())+
    scale_size_manual(values=rep(3, times=length(shapeScheme())))+
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    
    #coord_cartesian(xlim = c(0, 85)) + 
    #coord_cartesian(ylim=c(0.8, 1E+3)) +  
    
    xlab(paste0(pk_label, " (mg/L)")) + 
    ylab(paste0(pd_label, " (unit)")) + 
 
    theme_bw() +   
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))   
  
  
  fig = fig + annotation_logticks(sides ="b") +  # "trbl", for top, right, bottom, and left.
    theme(panel.grid.minor = element_blank())
  fig
 
  #------------------
  # linear scale
  #------------------
  attr(fig, 'title') <-  paste0("Percent Change from Baseline vs Concentration Regardless of Time Points and Dose Groups in Log-Log Scale in Analysis Set")
  figure[["SIGMOID_PKPD_LOG_LOG"]] = fig  
    
  
  return(list(data=data, figure=figure, table=table))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
output = suppressWarnings(
  filtered_dataset() %>% descriptive_PKPD_sigmoid(params=NULL)
)

}
