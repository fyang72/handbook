##########################################################################################
# simdata_plot_VPC
##########################################################################################

simdata_plot_VPC <-function(dataset, params=NULL) {
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
  tdata =  dataset %>% 
    filter(EVID==0) %>%
    mutate(TIME=TIME/7, 
           DVOR=IPRED) 
  
  # calculate stats
  tdata =  tdata %>% 
    calc_stats(id="ID", 
               group_by=c("STUDYID", "ARMA", "TIME", "TEST"), 
               value="DVOR") %>% 
    select(STUDYID, ARMA, TIME, N, Mean_CV, SE, SD, Median, PCT2P5, PCT97P5, GEOmean, GEOSD)   
   
  # plot it 
  tdata = tdata %>% mutate(
    xvar = TIME, 
    yvar = Median
  )
  
  x=setup_scale(myscale='1_4', 
                mylimit=c(0, max(tdata$xvar, na.rm=TRUE))
  )
  
  fig = ggplot(tdata, aes(x=xvar, y=yvar, group=ARMA, col=ARMA)) + 
    geom_line() + #col="black", show.legend=TRUE  ) +
    geom_ribbon(aes(ymin=PCT2P5,ymax=PCT97P5), fill="gray50", alpha="0.5", show.legend=F)   + 
    scale_fill_manual(values=c(clear,blue)) + 
    
    xlab("Time (week)") + 
    ylab("Predicted Concentration (mg/L)") + 
    
    base_theme(font.size=12) + 
    scale_x_continuous(breaks=x$breaks, label=as.character(x$labels)) + 
  
    #coord_cartesian(xlim = xlim) + 
    #coord_cartesian(ylim = ylim) + 
     
    facet_wrap(~ARMA)
  
  #------------------
  # VPC (95%)
  #------------------
  attr(fig, 'title') <- paste0(
    "Predicted Concentration with 95% Predicted  Confidence Interval")
  attr(fig, 'width') <- 9
  attr(fig, 'height') <- 6                                
  figure[["pk_VPC"]] = fig 
  figure[["pk_VPC"]]$data =  tdata
  
  return(list(figure=figure, table=table, message=message))
}

#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(simdata_plot_VPC(dataset, params=NULL))
  
}
