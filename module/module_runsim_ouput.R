
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

  uiOutput(ns("run_simulation_by_script_container"))
  
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
output$run_simulation_by_script_container<- renderUI({  
  
validate(
  need(cppModel(), message="No cppModel"), 
  need(values$adsl, message="No population (adsl)"), 
  need(values$adex, message="No dosing regimen (adex)")
  
  #need(input$infusion_hrs_lst, message="missing infusion hours"), 
  #need(input$seed, message="missing seed variable"), 
  #need(input$simulation_delta, message="missing simulation delta"), 
  #need(input$followup_period, message="missing followup_period")
)

# script
script <- list.files(path=paste0(HOME, "/script/"), 
                     full.names = TRUE,
                     pattern="runSim_by_script")
names(script) = basename(script)
 
# call module_run_script
ALL =  callModule(module_run_script, "mYtEsT_for_run_simulation_by_script", ALL, 
                  dataset= ALL$DATA[["mYtEsT_for_runSim_dataset"]], 
                  script = script, 
                  params = reactive(
                    list(
                      cppModel = cppModel(),
                      adex = values$adex,
                      adsl = values$adsl, 
                      YesNoIIV = input$YesNoIIV, 
                      simulation_delta = input$simulation_delta, 
                      followup_period = input$followup_period, 
                      infusion_hrs_lst = input$infusion_hrs_lst,
                      seed = input$seed
                      )
                    )
)

module_run_script_UI(ns("mYtEsT_for_run_simulation_by_script"), label = NULL) 

})

# addPercent <- function(x, mult = 100, ...){  06/28/2019, Friday, 08:45PM
#   percent <- round(x * mult, ...)
#   paste(percent, "%", sep = "")
# }
  
return(ALL)
}