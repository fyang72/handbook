 
##################################################### 
##################################################### 
#  xxxInput, xxxOutput, xxxControl, xxxUI, 
##################################################### 
##################################################### 

module_runsim_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  tabBox(width=12, id = ("run_cppModel"), title =NULL, 
    
    # cppModel_container 
    tabPanel(width=12, title="Load cppModel ", value = "load_cppModel", collapsible = TRUE, 
            collapsed = TRUE, solidHeader = TRUE,
            fluidRow(
              fluidRow(column(12, uiOutput(ns("load_cppModel_container")))),
              #fluidRow(column(12, uiOutput(ns("update_cppModel_container")))), 
            style='margin-bottom:30px;  border:1px solid; padding: 10px;'
            )
    ),       
    
    # adex_container 
    tabPanel(width=12, title="Dose regimen", value = "load_adex", collapsible = TRUE, 
            collapsed = TRUE, solidHeader = TRUE,
            fluidRow(column(12, uiOutput(ns('adex_container'))), 
                     style='margin-bottom:30px;  border:1px solid; padding: 10px;')
    ),    
    
    # adsl_container 
    tabPanel(width=12, title="Subject population", value = "load_adsl", collapsible = TRUE, 
            collapsed = TRUE, solidHeader = TRUE,
            fluidRow(column(12, uiOutput(ns("adsl_container"))), 
                     style='margin-bottom:30px;  border:1px solid; padding: 10px;')
    ), 
    
    # setting_container 
    # tabPanel(width=12, title="Simulation setting ", value = "simulation_setting", collapsible = TRUE, 
    #         collapsed = TRUE, solidHeader = TRUE,
    #         fluidRow(column(12, uiOutput(ns("setting_container"))), 
    #                  style='margin-bottom:30px;  border:1px solid; padding: 10px;')
    # ), 
    # 
    # run_simulation_container 
    tabPanel(width=12, title="Run simulation", value = "run_simulation", collapsible = TRUE, 
            collapsed = TRUE, solidHeader = TRUE,
            fluidRow(column(12, uiOutput(ns("run_simulation_container"))), 
                     style='margin-bottom:30px;  border:1px solid; padding: 10px;')
    )
  )
}


#####################################################  
##################################################### 
# module_runsim
##################################################### 
##################################################### 
  
module_runsim <- function(input, output, session, 
                          ALL,  cppModel_name)  {

ns <- session$ns
values <- reactiveValues(simData=NULL)
 
################################
# UI for load cpp model
################################
output$load_cppModel_container <- renderUI({ 
  
  ALL = callModule(module_load_cppModel, "load_cppModel_for_simulation", 
                   ALL, cppModel_name)

  module_load_cppModel_UI(ns("load_cppModel_for_simulation"), label = NULL)
})

################################
# UI for adsl_container
################################
output$adsl_container <- renderUI({ 
  
  values = callModule(module_runsim_adsl, "adsl_for_simulation", ALL, values)
  
  module_runsim_adsl_UI(ns("adsl_for_simulation"), label = NULL)
})

################################
# UI for adex_container
################################
output$adex_container <- renderUI({ 
  
  values = callModule(module_runsim_adex, "adex_for_simulation", ALL, values)
  
  module_runsim_adex_UI(ns("adex_for_simulation"), label = NULL)
})

################################
# UI for setting_container
################################
output$setting_container <- renderUI({ 
  
  values = callModule(module_runsim_setting, "setting_for_simulation", ALL, values)
  
  module_runsim_setting_UI(ns("setting_for_simulation"), label = NULL)
})


 #####################################
# UI for run_simulation_container
#####################################
output$run_simulation_container <- renderUI({ 
  
  ALL = callModule(module_runsim_output, "output_for_simulation", 
                      ALL, values, cppModel_name)
  
  module_runsim_output_UI(ns("output_for_simulation"), label = NULL)
})

return(ALL)
}
