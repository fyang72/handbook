
diagnostic_GOF3 <- function(dataset, params=NULL, 
                            n_subject_in_panel = 9)  {
  
  xpdb = dataset
  
  # dataset = read_csv("./data/nmdat_0226_2019.csv", col_names=TRUE,  
  #                  col_type=cols(.default=col_character()))  # read as character  
  
  figure=NULL
  table =NULL
  data = NULL
  
  #--------------------------------------
  # setup  
  #--------------------------------------
  tdata0 = slot(xpdb, "Data")    # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno
  
  # Need key variables
  if (!all(c("PRED", "IPRED", "DV", "ID", "TIME") %in% colnames(tdata0))) {
    # c("default", "message", "warning", "error"), 
    showNotification("need PRED, IPRED, DV, ID, and TIME in the xpdb@Data", type = "error")
  }
  
  validate(need(all(c("PRED", "IPRED", "DV", "ID", "TIME") %in% colnames(tdata0)), 
                message="need PRED, IPRED, DV, ID, and TIME in the data")
  )
  
  #-------------------
  # individual plots for all indiv
  #-------------------
  #n_subject_in_panel = 9   # global parameter, number of fig in each panel
  ID <- unique(tdata0$ID)
  ID_lst <- split(ID, ceiling(seq_along(ID)/n_subject_in_panel))
  
  tdata0 = slot(xpdb, "Data")
  for (i in 1:length(ID_lst)) {          
    # values4xpdb = xpdb_diagnostic_GOF3(xpdb, values4xpdb=NULL, 
    #                                    n=64, 
    #                                    ids=tdata %>% filter(group==i)%>%pull(ID),
    #                                    font.size=10)
    # 
    #  
    tdata = tdata0 %>% mutate(xvar=TIME, yvar=DV) %>%  # brush show the observed data
      filter(ID %in% ID_lst[[i]])
    
    fig = ggplot(tdata,aes(x=xvar,y=yvar))+ 
      geom_point()+ 
      facet_wrap(~ID)+
      geom_line(aes(x=xvar,y=IPRED, color="Individual Prediction"),lwd=0.7) +
      geom_line(aes(x=xvar,y=PRED, color="Population Prediction"),lwd=0.7) +  
      scale_colour_manual("", 
                          #breaks = c("darkorange", "blue"),
                          values = c("Individual Prediction"="darkorange", 
                                     "Population Prediction"="blue" )
      ) + 
      
      ylab("Observed & model prediction(mg/L)") +   
      theme_bw()+ base_theme(font.size=12) + 
      xlab(expression(Time~(days))) 
    
    # meta
    attr(fig, 'title') <- paste0("Model Fitting of Predicted/Observed Concentration-time Profiles")
    attr(fig, 'width') <- 9
    attr(fig, 'height') <- 6                             
    figure[[paste0("indivfit", i)]]  = fig 
    figure[[paste0("indivfit", i)]] $data =  tdata
    figure[[paste0("indivfit", i)]] $dataset = slot(xpdb, "Data")  # or xpdb
  }
  
  return(list(data=data, figure=figure, table=table))
}



#################################################################
# final output
#################################################################
if (ihandbook) {
  output = suppressWarnings(diagnostic_GOF3(dataset, params=NULL))
}