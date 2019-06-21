
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
  #fluidRow(
    column(width=4, #offset = 1, 
         radioButtons(ns("YesNoIIV"), 
                      label = "Inter-Individual Variability (IIV)", 
                      inline = TRUE,
                      choices = list("Yes" = "Yes", 
                                     "No" = "No"), 
                      width = "100%",
                      selected = "No")
         ),
    
    column(width=2, numericInput(ns("seed"), label = "Random seed",
                                  value=1234, min=1, max=10000)
    ),
    
    column(width=2, numericInput(ns("infusion_hrs_lst"), label = "Infusion Hrs",
                                 value=1, min=0, max=10)
    ),
    
    column(width=2, numericInput(ns("simulation_delta"), label = "Sim_delta(day)",
                                 value=1, min=0.1, max=10)
    ),
    
    column(width=2, numericInput(ns("followup_period"), label = "Followup(day)",
                                 value=112, min=1, max=448)
    )
    # tgrid
    
    ), 
  # 
  # fluidRow(
  #   column(width=4,  
  #          actionButton(ns("run_simulation"), label="Run simulation", style=actionButton.style ) 
  #   ),
  #   
  #   column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
  #          textInput(ns("data_name"), value="simDat-", label=NULL)),
  #   
  #   column(width=4, #status = "primary",  #class = 'rightAlign',#background ="aqua",
  #          actionButton(ns("save_simdata"),label="Save data", style=actionButton.style))
  # ), 
  #style='margin-bottom:30px;  border:1px solid; padding: 10px;' 
  #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
  #), 
  
  uiOutput(ns("run_simulation_by_script_container"))
  #fluidRow(column(12, uiOutput(ns("run_simulation_by_script_container"))))
  
  # fluidRow(
  #   column(width=12, uiOutput(ns("simulated_profile_linear_container")))
  # ), 
  # fluidRow(
  #   column(width=12, uiOutput(ns("simulated_profile_log_container")))
  # )
)
}


########################################################################
# module_runsim_adex
########################################################################

module_runsim_output <- function(input, output, session, ALL, values, cppModel_name)  {
  
  ns <- session$ns 
  
  ################################################
  # UI for run_simulation_by_script_container
  ################################################  
  output$run_simulation_by_script_container<- renderUI({  
    
    values$dataset <- ALL$DATA[["mYtEsT_for_runSim_dataset"]]
    values$cppModel = cppModel()
    #values$adex
    #values$adsl
     
    values$YesNoIIV = input$YesNoIIV  
    values$seed = input$seed  
    values$infusion_hrs_lst = input$infusion_hrs_lst   
    values$simulation_delta = input$simulation_delta
    values$followup_period = input$followup_period
    
    validate(
      need(values$cppModel, message="No cppModel"), 
      need(values$adsl, message="No population (adsl)"), 
      need(values$adex, message="No dosing regimen (adex)"),
      
      need(values$infusion_hrs_lst, message="missing infusion hours"), 
      need(values$seed, message="missing seed variable"), 
      need(values$simulation_delta, message="missing simulation delta"), 
      need(values$followup_period, message="missing followup_period")
    )
    
    script <- list.files(path=paste0(HOME, "/script/"), 
                         full.names = TRUE,
                         pattern="runSim_by_script")
    names(script) = basename(script)
    
    ALL =  callModule(module_run_script, "mYtEsT_for_run_simulation_by_script", ALL, 
                      dataset=values$dataset, 
                      script = script, 
                      params = values)
    
    module_run_script_UI(ns("mYtEsT_for_run_simulation_by_script"), label = NULL) 
    
  })
  
  
   
#--------------------
# cppModel 
#--------------------  
cppModel <- reactive({
  cppModel =  ALL$cppModel[[cppModel_name]]
  
  # if (is.null(cppModel)) {
  #   # "default, "message", "warning", "error"  
  #   showNotification("No valid cppModel found", type="error")  
  # }
  validate(need(cppModel, message="no cppModel found"))
  
   if (input$YesNoIIV=="No") {
      cppModel  =  cppModel  %>% zero_re  
     message = "Simulation with no inter-individual variability (IIV)"
     showNotification(message, type="message")  # "default, "message", "warning", "error"  
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
  
  values$simData <- runSim_by_dosing_regimen(
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
  
  newData <- values$simData 
  data_name <- input$data_name
  #names(newData) = data_name
  
  attr(newData, "data.name") = data_name
  ALL$DATA[[data_name]] <- newData
  
  # "default, "message", "warning", "error"  
  showNotification("Data saved", type="message")
})

return(ALL)
}