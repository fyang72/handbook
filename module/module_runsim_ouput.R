
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
                      selected = "Yes")
         ),
    
    column(width=4, numericInput(ns("seed"), label = "Random seed:",
                                  value=1234, min=1, max=10000)
    ),
    
    column(width=4, numericInput(ns("infusion_hrs_lst"), label = "Infusion Hour",
                                 value=1, min=0, max=10)
    )
    
    ), 
  
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
    fig = ggplot(tdata, aes(x=xvar, y=yvar, group=ID, col=ARMA)) + 
      #ggtitle("Concentration Time Profile") + 
      
      geom_point() + geom_line() +   
      #geom_errorbar(aes(ymin = meanMinusSE, ymax = meanPlusSE), width=0.2) + 
      
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
  cppModel = cppModel() 
  adsl = adsl();  
  adex = adex()  
  
  validate(need(cppModel, message="no model loaded"), 
           need(adsl, message="no adsl loaded"), 
           need(adex, message="no adex loaded"), 
           need(input$infusion_hrs_lst, message=FALSE)
           )
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
  progress$set(message = "Running Simulation...Please Wait", value = 0)
   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (1==2) { 
    cppModel = mread(model='cppModel', project=paste0(HOME, '/model/cpp/'),file="LN001.cpp")
    adsl = data.frame(ID=c(1,2), WGTBL=75)  # a data.frame
    adex = parseARMA(c("3 mg/kg IV Q2W*12 ", "3 mg/kg IV QW*1 + 350 mg IV Q3W*8"))            # a data.frame or a event object
       
    seed = 1234
    simulation_delta = 1
    followup_period = 112
    infusion_hrs = 1 
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  infusion_hrs_lst = input$infusion_hrs_lst
  seed = input$seed
  
  values$simData <- runSim_by_dosing_regimen2(
    cppModel,    # model file
    adsl,   # population 
    adex,   # dose regimen
    simulation_delta = simulation_delta,  # integration step                  
    tgrid = NULL,     # extra timepoint (other than delta)
    infusion_hrs = infusion_hrs_lst,  # hour,  infusion hours
    followup_period = followup_period,   # how long of the followup period after treatment
    seed=seed)
     
}) 



#-------------------------------------------------------------
# calculate the statistic summary table based on simulated data
#-------------------------------------------------------------
statsTab <- reactive({ 
  
  #print("before simulation:")    
  tdata = values$simData # run_simulation()
  validate(need(tdata, message="no simdata found"))
  
  subj.lst = sample(unique(tdata$ID), 
                    min(n_subject_showing_in_simulation, 
                        length(unique(tdata$ID))
                        )
                    )
  
  tdata = tdata %>% 
    filter(ID %in% subj.lst) %>% 
    mutate(xvar=as_numeric(TIME), 
           yvar=as_numeric(IPRED))
   
  tdata 
  
  # 
  # statsTab = tdata %>% select(one_of("ARMA", "ID", "NTIM", "DVOR"))  %>% 
  #   calc_stats(id="ID", group_by=c("ARMA",  "NTIM"), value="DVOR")  %>%
  #   filter(!is.na(NTIM) & !is.na(Mean))
  #    
  # EPS= 0.078/2
  # tdata = statsTab %>% 
  #   mutate(NTIM=NTIM+EPS, 
  #          Mean=Mean+EPS, 
  #          meanPlusSE=meanPlusSE+EPS, 
  #          meanMinusSE=meanMinusSE+EPS) %>% 
  #   mutate(xvar=as_numeric(NTIM), 
  #          yvar=as_numeric(Mean))
  # 
  # tdata 
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