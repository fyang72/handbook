
#######################################################################
# module_build_adex_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_runsim_output_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
tagList(
  fluidRow(
  fluidRow(
    column(width=4, #offset = 1, 
         radioButtons(ns("YesNoIIV"), 
                      label = "Inter-Individual Variability (IIV)", 
                      inline = TRUE,
                      choices = list("Yes" = "Yes", 
                                     "No" = "No"), 
                      width = "100%",
                      selected = "Yes")),
    
    column(width=4, numericInput(ns("seed"), label = "Random seed:",
                                  value=1234, min=1, max=10000)
    )), 
  
  fluidRow(
    column(width=4,  
           actionButton(ns("run_simulation"), label="Run simulation", style=actionButton.style ) 
    ),
    
    column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
           textInput(ns("data_name"), value="simDat-", label=NULL)),
    
    column(width=4, #status = "primary",  #class = 'rightAlign',#background ="aqua",
           actionButton(ns("save_simdata"),label="Save data", style=actionButton.style))
  ), 
  style='margin-bottom:30px;  border:1px solid; padding: 10px;' 
  #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
  ), 
  
  fluidRow(
    column(width=12, uiOutput(ns("simulated_profile_container")))
  )
)
}



########################################################################
# module_runsim_adex
########################################################################

module_runsim_output <- function(input, output, session, ALL, values, cppModel_name)  {
  
  ns <- session$ns 
  
  ################################
  # UI for setting_container
  ################################
  output$simulated_profile_container <- renderUI({ 
    
    tdata = statsTab()
    validate(need(tdata, message= "no data generated yet"))
    
    
    x=setup_scale(myscale='1_2', mylimit=c(0, max(tdata$xvar/7, na.rm=TRUE)))
    
    # no pre-dose samples in plot
    tdata = tdata%>%mutate(xvar=xvar/7)
    fig = ggplot(tdata, aes(x=xvar, y=yvar, group=ARMA, col=ARMA)) + 
      #ggtitle("Concentration Time Profile") + 
      
      geom_point() + geom_line() +   
      geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
      
      scale_color_manual(values=colScheme()) +  
      
      scale_x_continuous(breaks=x$breaks, label=x$labels) +
      #scale_y_continuous(breaks=y$breaks, label=y$labels) +
      
      #coord_cartesian(xlim = c(0, 85)) + 
      
      xlab("Time (week)") +  
      ylab(paste0("Concentration(±SE) (mg/L)")) +   
      
      theme_bw() + base_theme(font.size = as.integer(12)) + 
      guides(col=guide_legend(ncol=4,byrow=TRUE))    
     
   
    attr(fig, "title") = "Predicted concentration (mg/L)" 
    ALL = callModule(module_save_figure, "simulated_profile", ALL, 
                         figure=fig, 
                         figure_index = 1, 
                         figure_name="fig-", 
                         figure_data = tdata
                         )
     
     fluidRow(column(12, module_save_figure_UI(ns("simulated_profile"), label = NULL)))
  })
  
   
#--------------------
# cppModel 
#--------------------  
cppModel <- reactive({
  cppModel =  ALL$cppModel[[cppModel_name]]
  
  if (is.null(cppModel)) {
    # "default, "message", "warning", "error"  
    showNotification("No valid cppModel found", type="error")  
  }
  validate(need(cppModel, message="no cppModel found"))
  
  if (input$YesNoIIV=="No") {
     cppModel  =  cppModel  %>% zero_re  
    #message = "Simulation with no inter-individual variability (IIV)"
    # "default, "message", "warning", "error"  
    #showNotification(message, type="message")  
  }
  
   
  
  cppModel
})


#--------------------
# adex 
#--------------------  
adex <- reactive({
  adex =  values$adex
  if (is.null(adex)) {
    # "default, "message", "warning", "error"  
    showNotification("No valid adex found", type="error")  
  }
  validate(need(adex, message="no adex found"))
  
  adex
})

#--------------------
# adsl
#--------------------  
adsl <- reactive({
  adsl =  values$adsl
  validate(need(adsl, message="no adsl found"))
  
  adsl
})

#--------------------
# run_simulation
#-------------------- 
 
observeEvent(input$run_simulation, {
  
  # load  
  mod = cppModel() 
  adsl = adsl();  
  adex = adex()  
  validate(need(mod, message="no model loaded"), 
           need(adsl, message="no adsl loaded"), 
           need(adex, message="no adex loaded")
           )
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
  progress$set(message = "Running Simulation...Please Wait", value = 0)
  
  
  # setup parameters for simulation
  # -----------------------------------
  seed = input$seed
  delta = values$delta
  followup_period = values$followup_period
  infusion_hrs_lst = values$infusion_hrs_lst
   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (1==2) { 
  mod = mread("./model/cpp/LN001.cpp")
  adsl = data.frame(ID=c(1,2), WGTBL=75)  # a data.frame
  adex = parseARMA(c("3 mg/kg IV Q2W*12", "350 mg IV Q3W*8"))            # a data.frame or a event object
     
  seed = 1234
  delta = 1
  followup_period = 112
  infusion_hrs_lst = 1 
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  adex = adex %>%          # a data.frame or a event object
    capitalize_names()
   
  adex = adex %>% left_join(adsl, by="ID")
  
  # POP + REP + ID + DOSEID(ARMA) + TIME
  adex = adex %>% mutate(AMT = ifelse(UNIT=="mg/kg", AMT * as_numeric(WGTBL), AMT), 
                         CMT = ifelse(ROUTE=="IV", 2, 
                                      ifelse(ROUTE=="SC", 1, 0)), 
                         RATE = ifelse(ROUTE=="IV", AMT/(infusion_hrs_lst/24), 0), 
                         
                         ROUTEN = ifelse(ROUTE=="IV", 1, 
                                         ifelse(ROUTE=="SC", 2, 0))) 
  
  
  
  # set seed to make reproducible simulation (if IIV=1)
  set.seed(seed)
  
  # library("PKPDmisc")
  treat_end1 = ifelse(all(c("ADDL", "II") %in% colnames(adex)), max((adex$ADDL+1)*adex$II), 0) 
  treat_end2 = adex %>% pull(TIME) %>% as_numeric() %>% max(na.rm=TRUE) # 
  treat_end = max(treat_end1, treat_end2)
  
  sim_end = treat_end + followup_period   # default 112 days  
   
  
  # note dose by week only
  tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7))
 
  # run simulation
  # -----------------------------------
  out = mod %>% data_set((adex)) %>% 
    mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>% 
    as.data.frame() %>% capitalize_names()  %>% slice(2:n())
  
  # add back ID, STUDYID, USUBJID, ARMA, and EXROUTE
  add_col_lst = c("ARMA", "ROUTE", "WGTBL")
  out = out[, setdiff(colnames(out), add_col_lst)]
  out = out  %>% 
    left_join(adex %>% as.data.frame() %>% 
                distinct(ID, .keep_all=TRUE) %>% 
                select(ID, ARMA, ROUTE, WGTBL),
              by=c("ID"))  
  
  col.lst <- colnames(out)[which(substr(colnames(out), 1,6) == "IPRED_")]
  out = out %>% gather(TEST, IPRED, -one_of(setdiff(colnames(out), col.lst))) %>% 
    mutate(TEST=gsub("IPRED_", "", TEST, fixed=TRUE))
  
  # col.lst <- colnames(out)[which(substr(colnames(out), 1,3) == "DV_")]
  # out = out %>% gather(TEST, DVOR, -one_of(setdiff(colnames(out), col.lst)))
  # 
  
  # clean ARMA
  #out = out %>% mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE))
  
  # EXSEQ
  out = out %>% #mutate(EVID=ifelse(TAD==0, 1, 0))   %>% 
    group_by(ID) %>%  mutate(EXSEQ=cumsum(EVID))     # for calculat the last dose interval later
  
  # fill up II by  Last Observation Carried Forward
  # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
  out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% 
    group_by(ID) %>% fill(II) %>% 
    ungroup()
  
  head(out) %>% as.data.frame()
   
  # simData is a data.frame of 
  # c("STUDYID", "ARMA", "USUBJID", "ID", "TIME","NTIM", "TAD", "EVID", 
  #    "II", "TEST","DVOR", "EXSEQ", "WGTBL")
  values$simData = out
 
}) 



#-------------------------------------------------------------
# calculate the statistic summary table based on simulated data
#-------------------------------------------------------------
statsTab <- reactive({ 
  
  #print("before simulation:")    
  tdata = values$simData # run_simulation()
  validate(need(tdata, message="no simdata found"))
  
  tdata = tdata %>% mutate(NTIM=TIME, DVOR=IPRED) %>%   # use IPRED as DVOR
    mutate(NTIM=as_numeric(NTIM), 
           DVOR=as_numeric(DVOR))
  
  statsTab = tdata %>% select(one_of("ARMA", "ID", "NTIM", "DVOR"))  %>% 
    calc_stats(id="ID", group_by=c("ARMA",  "NTIM"), value="DVOR")  %>%
    filter(!is.na(NTIM) & !is.na(Mean))
     
  EPS= 0.078/2
  tdata = statsTab %>% 
    mutate(NTIM=NTIM+EPS, 
           Mean=Mean+EPS, 
           meanPlusSE=meanPlusSE+EPS, 
           meanMinusSE=meanMinusSE+EPS) %>% 
    mutate(xvar=as_numeric(NTIM), 
           yvar=as_numeric(Mean))
  
  tdata 
})


#-------------------------------------------------------------
# save simulated data
#-------------------------------------------------------------
#add figure to log when action button is pressed
observeEvent(input$save_simdata, {
  #req(input$addToCart, FIGURE_ALL, input$fig_name)
  
  if(is.null(input$save_simdata))  {return()}
  if(input$save_simdata == 0) {return() }
  cat(file=stderr(), "##############Step: save_simdata #################", "\n")
  
  newData <- values$simData 
  data_name <- input$data_name
  #names(newData) = data_name
  
  attr(newData, "data.name") = data_name
  ALL$DATA[[data_name]] <- newData
  
})

return(ALL)
}