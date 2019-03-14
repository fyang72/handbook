##########################################################################################
# simdata_summary_exposure
##########################################################################################

simdata_summary_exposure <-function(dataset, params=NULL) {
  #dataset=read_csv("./data/nmdatPKPD.csv") %>% filter(TESTCAT=="TARGET")
  
  figure=NULL
  table =NULL
  data = NULL
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst <- c("ID", "ARMA", "TEST", "IPRED", "TIME", "ROUTE")
  missing.column.lst <- key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message <- paste0("missing variable(s) of ", paste0(missing.column.lst, sep=", ", collapse=""))
  
  validate(need(all(key.column.lst %in% colnames(dataset)), message=message)
  )
  
  
  #------------------------------           
  # prepare the dataset 
  #------------------------------
  tdata = dataset  %>%  
    mutate(DVOR=as_numeric(IPRED),  
           TIME=as_numeric(TIME), 
           ARMA=ordered(ARMA, levels=unique(ARMA))
    ) %>% 
    filter(TIME>=0)  
  tdata$STUDYID = "SFS"
  
  group_lst = c("STUDYID", "ARMA" )
  tdata = tdata %>% group_by(.dots = c(group_lst, "ID")) %>% 
    dplyr::summarise(CMAX = max(DVOR, na.rm=TRUE), 
                     CMIN = min(DVOR, na.rm=TRUE), 
                     AUC = auc_partial(TIME, DVOR)
    ) %>% 
    gather(TEST, DVOR, -one_of(group_lst), -ID)
   param_lst = c("CMIN", "CMAX", "AUC")
  
  tabl = tdata %>% group_by(.dots = c(group_lst, "TEST")) %>% 
    dplyr::summarise(N=length(unique(ID)), 
                     Mean = mean(DVOR, na.rm=TRUE), 
                     Median = median(DVOR, na.rm=TRUE),
                     SD = sd(DVOR, na.rm=TRUE), 
                     SE = sd(DVOR,na.rm=TRUE)/sqrt(length(DVOR))
                     
    ) %>% 
    mutate(TEST = ordered(TEST, levels=param_lst)) %>% 
    arrange(TEST)
  stats_lst = c("N", "Mean", "SE","SD", "Median")
  
  #------------------
  # long format 
  #------------------
  attr(tabl, 'title') <-  paste0("Summary of Exposure by Dose Group")
  table[["summary_exposure_long"]] = tabl 
  attr(tabl, 'footnote') <-  paste0("QW: weekly, Q2W: bi-weekly")
  
  #------------------
  # wide format 
  #------------------
  tabl = tabl %>% spread_n(TEST, c(N, Mean, SE, SD, Median)) %>%
    order_cols(group_lst, param_lst, stats_lst) 
  attr(tabl, 'title') <-  paste0("Summary of Exposure by Dose Group")
  table[["summary_exposure_wide"]] = tabl 
  attr(tabl, 'footnote') <-  paste0("QW: weekly, Q2W: bi-weekly")
  
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = simdata_summary_exposure(dataset, params=NULL)
  
}
