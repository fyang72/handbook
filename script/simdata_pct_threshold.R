##########################################################################################
# simdata_pct_threshold
##########################################################################################

simdata_pct_threshold <-function(dataset, params=NULL) {
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
    cppModel_file ="/home/feng.yang/handbook/model/cpp/LN001.cpp"
    cppModel=mread(model='cppModel',project=dirname(cppModel_file),file=basename(cppModel_file))
    
    adsl = data.frame(ID=seq(1,10,by=1), WGTBL=75)  # a data.frame
    adex = parseARMA(c("3 mg/kg IV Q2W*12 ", "3 mg/kg IV QW*1 + 350 mg IV Q3W*8")) # a data.frame or a event object
    
    seed = 1234
    simulation_delta = 1
    followup_period = 112
    infusion_hrs = 1 
  
    # duplicated simulations
    nrep = 10
    nsubject = nrow(adsl)
    
    dataset = lapply(1:nrep, function(i) {
      print(i)
      
      # virtual subject 
      idata = adsl %>% 
        sample_n(size=nsubject, replace=TRUE) %>% 
        mutate(ID = 1:nsubject)   
      
      # run simulation   
      cbind(REP=i,
            runSim_by_dosing_regimen2(cppModel, 
                                     idata, 
                                     adex, 
                                     simulation_delta = 0.1,# default density of time point                           
                                     tgrid = NULL,  # extra timepoint (other than delta)
                                     infusion_hrs = 1,
                                     followup_period = 84,
                                     seed=sample(1:1000, 1)) %>% as.data.frame() 
      )
    }) %>% bind_rows()
    
    
  } 
  
  #------------------------------
  # these key varaibles needed
  #------------------------------
  key.column.lst<-c("ID","ARMA","TEST","IPRED","TAD","TIME","II","EXSEQ")
  missing.column.lst<-key.column.lst[which(!key.column.lst %in% colnames(dataset))]
  message<-paste0("missing variable(s) of ", paste0(missing.column.lst, sep=", ", collapse=""))
  
  validate(need(all(key.column.lst %in% colnames(dataset)), message=message)
  )
  
    
  
  ############################################################## 
  # calculate the percentage of patient above 
  # conc threshold at treatmetn period
  ##############################################################
  tdata = dataset %>% filter(TIME==24*7)  %>%    # at week 24
    
    mutate(CONC_GT_100 = ifelse(CP>=100, 1, 0), 
           CONC_GT_150 = ifelse(CP>=150, 1, 0), 
           CONC_GT_200 = ifelse(CP>=200, 1, 0)
    )
  
  # percentage of population above theshold concentration  
  #-------------------------------------------------------
  #tdata$REP = 1  # number of replicates
  
  tdata = tdata %>% 
    group_by(REP, ARMA) %>% 
    summarise(N=length(unique(ID)), 
              PCT_CONC_GT_100 = sum(CONC_GT_100)/N*100, 
              PCT_CONC_GT_150 = sum(CONC_GT_150)/N*100,  
              PCT_CONC_GT_200 = sum(CONC_GT_200)/N*100  
  ) %>% 
    gather(TEST, DVOR, -REP, -ARMA, -N)

  #  calculate stats
  #---------------------- 
  tabl = tdata  %>%    # exclude pre-dose samples
    
    # calculate the statistics (Mean, SE, SD)
    calc_stats(id="REP", 
               group_by=c("ARMA",  "TEST"), 
               value="DVOR") %>% 
    
    select(ARMA, TEST, N,
           #Mean, meanMinusSE, meanPlusSE, 
           Mean_SD, Mean_SE, Median_Range) %>% 
    arrange(ARMA, TEST)
  
  tabl
  
  # paste0(round(sum(CONC_GT_200)/N*100,digits=2),"%")
  attr(tabl, 'title') <- "Percentage of Subject Achieving threshold Concentration for Different Dosing Regimens at Week24" 
  attr(tabl, 'footnote') <-  paste0("QW: weekly, Q2W: bi-weekly")
  table[["CONC_PCT_WK24"]] = tabl
  
    
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = simdata_summary_exposure(dataset, params=NULL)
  
}