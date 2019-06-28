
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
                                 value=1, min=0, max=24)
    ),
    
    column(width=2, numericInput(ns("simulation_delta"), label = "Sim_delta(day)",
                                 value=1, min=0.1, max=10)
    ),
    
    column(width=2, numericInput(ns("followup_period"), label = "Followup(day)",
                                 value=112, min=1, max=448)
    )
    # tgrid
    
    ), 
    
  #uiOutput(ns("run_simulation_by_script_container"))
  module_run_script_UI(ns("mYtEsT_for_run_simulation_by_script"), label = NULL) 
  
  #fluidRow(column(12, uiOutput(ns("run_simulation_by_script_container"))))
)
}

########################################################################
# module_runsim_adex
########################################################################

module_runsim_output <- function(input, output, session, ALL, values, cppModel_name)  {
  
  ns <- session$ns 
  
#--------------------
# cppModel 
#--------------------  
cppModel <- reactive({
  validate(need(cppModel_name, message="no cppModel found"))
  
  cppModel =  ALL$cppModel[[cppModel_name]]
  validate(need(cppModel, message="no cppModel found"))
  # "default, "message", "warning", "error"  
  #if (is.null(cppModel)) {showNotification("No valid cppModel found", type="error")}   
  
  #validate(need(input$YesNoIIV, message=FALSE))
   
  
   if (!is.null(input$YesNoIIV) && input$YesNoIIV=="No") {
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
  # "default, "message", "warning", "error"  
  if (is.null(adex)) {showNotification("No valid adex found", type="error")}
  validate(need(adex, message="no adex found"))
  
  adex
})

#--------------------
# adsl
#--------------------  
adsl <- reactive({
  adsl =  values$adsl
  # "default, "message", "warning", "error"  
  if (is.null(adsl)) {showNotification("No valid adsl found", type="error")}
  validate(need(adsl, message="no adsl found"))
  
  adsl
})



################################################
# UI for run_simulation_by_script_container
################################################  
#output$run_simulation_by_script_container<- renderUI({  
params = NULL

params$dataset <- ALL$DATA[["mYtEsT_for_runSim_dataset"]]
params$cppModel = isolate({cppModel()})
params$adex = isolate({values$adex})
params$adsl = isolate({values$adsl})

  
validate(
  need(params$cppModel, message="No cppModel"), 
  need(params$adsl, message="No population (adsl)"), 
  need(params$adex, message="No dosing regimen (adex)")
  
  # need(params$infusion_hrs_lst, message="missing infusion hours"), 
  # need(params$seed, message="missing seed variable"), 
  # need(params$simulation_delta, message="missing simulation delta"), 
  # need(params$followup_period, message="missing followup_period")
)

script <- list.files(path=paste0(HOME, "/script/"), 
                     full.names = TRUE,
                     pattern="runSim_by_script")
names(script) = basename(script)


if (1==2) {
  cppModel_file = "LN001.cpp"
  cppModel=mread(model='cppModel', 
                 project=paste0(HOME, '/cpp/'), quiet=TRUE, 
                 file=basename(cppModel_file))
  adex <- parseARMA("1 mg/kg IV Q3W*2")
  adsl = data.frame(USUBJID=1:3, WGTBL=seq(60, 80, by=10))
  
  # runSim_by_dosing_regimen    
  # ---------------------------------------  
  if (which_mode==1) { 
    simData = suppressWarnings(runSim_by_dosing_regimen(
      cppModel,    # model file
      adsl,   # population 
      adex,   # dose regimen
      simulation_delta = 1, #simulation_delta,  # integration step                  
      tgrid = NULL,     # extra timepoint (other than delta)
      infusion_hrs_lst = 1, #infusion_hrs_lst,  # hour,  infusion hours
      followup_period = 123, # params(), #followup_period,   # how long of the followup period after treatment
      seed=1234
    ))  
    
    output = suppressWarnings(postProcess_simData(simData, params))
  }
  
}


ALL =  callModule(module_run_script, "mYtEsT_for_run_simulation_by_script", ALL, 
                  dataset= ALL$DATA[["mYtEsT_for_runSim_dataset"]], 
                  script = script, 
                  params = 234  #reactive(input$followup_period)
)

addPercent <- function(x, mult = 100, ...){
  percent <- round(x * mult, ...)
  paste(percent, "%", sep = "")
}

#new.numbers = c(0.34368768, 0.46435636)
#addPercent(new.numbers, digits = 4)


#module_run_script_UI(ns("mYtEsT_for_run_simulation_by_script"), label = NULL) 

#})


  
return(ALL)
}