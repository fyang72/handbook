##########################################################################################
# simdata_summary_exposure
##########################################################################################

simdata_summary_exposure <-function(dataset, params=NULL) {
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character 
  
  figure=NULL
  table =NULL
  data = NULL
  
  
  # load mrgsolve cpp model
  #-------------------------------------
  library(dplyr)
  library("mrgsolve") 
  
  if (1==2) { 
    #cppModel_file = "/home/feng.yang/handbook/model/cpp/LN001.cpp"
    cppModel_file = paste0(HOME, "/model/cpp/LN001.cpp")
    cppModel=mread(model='cppModel',project=dirname(cppModel_file),file=basename(cppModel_file))
    
    adsl = data.frame(ID=c(1,2), WGTBL=75)  # a data.frame
    adex = parseARMA(c("3 mg/kg IV Q2W*12 ", "3 mg/kg IV QW*1 + 350 mg IV Q3W*8"))            # a data.frame or a event object
    
    seed = 1234
    simulation_delta = 1
    followup_period = 112
    infusion_hrs = 1 
    
    dataset <- runSim_by_dosing_regimen2(
      cppModel,    # model file
      adsl,   # population 
      adex,   # dose regimen
      simulation_delta = 1,  # integration step                  
      tgrid = NULL,     # extra timepoint (other than delta)
      infusion_hrs = 1,  # hour,  infusion hours
      followup_period = 84,   # how long of the followup period after treatment
      seed=1234)
    
  } 
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst<-c("ID","ARMA","TEST","IPRED","TAD","TIME","II","ROUTE","EXSEQ")
  missing.column.lst<-key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message<-paste0("missing variable(s) of ", paste0(missing.column.lst, sep=", ", collapse=""))
  #validate(need(all(key.column.lst %in% colnames(dataset)), message=message))
  
  #------------------------------           
  # prepare the dataset 
  #------------------------------
  tdata = dataset  %>%  
    mutate(DVOR=as_numeric(IPRED),  
           TIME=as_numeric(TIME), 
           ARMA=ordered(ARMA, levels=unique(ARMA)) 
    ) %>% 
    filter(TIME>=0)  
  
  group_lst = c("STUDYID", "ARMA", "TEST", "ID") #TEST:[PK,PD]
  
  # filter out the last dosing interval
  tdata = tdata  %>%  
    group_by_(.dots =group_lst) %>%  
    top_n(n=1, wt=EXSEQ) %>% filter(TAD<=II)
  
  tdata = tdata %>% group_by(.dots = group_lst) %>% 
    dplyr::summarise(
      CMAX = round(max(DVOR, na.rm=TRUE), digits=3),
      CMIN = round(min(DVOR, na.rm=TRUE), digits=3),
      #CMIN = round(DVOR[n()],digits=3),   # the last conc at this time interval
      AUC = round(auc_partial(TIME, DVOR),digits=3)
    ) %>% 
    gather(TEST2, DVOR, -one_of(group_lst)) #TEST2:[CMAX,CMIN,AUC]
  param_lst = c("CMIN", "CMAX", "AUC")
  
  # Calculate stats based on dose groups, TEST, TEST2
  group_lst2 = c("STUDYID", "ARMA", "TEST", "TEST2")
  tabl = tdata %>% group_by(.dots = group_lst2) %>% 
    dplyr::summarise(N=length(unique(ID)), 
                     Mean = mean(DVOR, na.rm=TRUE), 
                     Median = median(DVOR, na.rm=TRUE),
                     SD = sd(DVOR, na.rm=TRUE), 
                     SE = sd(DVOR,na.rm=TRUE)/sqrt(length(DVOR))
                     
    ) %>% 
    mutate(TEST2 = ordered(TEST2, levels=param_lst)) %>% 
    arrange(TEST2)
  stats_lst = c("N", "Mean", "SE","SD", "Median")
  
  #------------------
  # long format 
  #------------------
  attr(tabl, 'title') <-  paste0("Summary of Exposure by Dose Group")
  attr(tabl, 'dataset') <- dataset
  if  (exists('input')) {
    attr(tabl, 'script') <- input$script_content
  }else{
    attr(tabl, 'script') <- NULL
    }
  attr(tabl, 'footnote') <-  paste0("QW: weekly, Q2W: bi-weekly")
  
  table[["summary_exposure_long"]] = tabl 
  
  #------------------
  # wide format 
  #------------------
  tabl = tabl %>% spread_n(TEST2, c(N, Mean, SE, SD, Median)) %>%
    order_cols(group_lst, param_lst, stats_lst) 
  attr(tabl, 'title') <-  paste0("Summary of Exposure by Dose Group")
  attr(tabl, 'dataset') <- dataset
  if (exists('input')) {
    attr(tabl, 'script') <- input$script_content
  }else{
    attr(tabl, 'script') <- NULL
  }
  attr(tabl, 'footnote') <-  paste0("QW: weekly, Q2W: bi-weekly")
  
  table[["summary_exposure_wide"]] = tabl 
  
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(simdata_summary_exposure(dataset, params=NULL))
  
}
