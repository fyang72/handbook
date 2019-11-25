#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 
  ######################################################################
  # setup
  ######################################################################
  #source(paste0(HANDBOOK_HOME, "/module/module_run_report.R")) 
  

 
  
  ######################################################################
  # testing
  ######################################################################
  library(shiny)
  library(shinydashboard)
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      #uiOutput("figure_container")  
      #fluidRow(column(12, uiOutput(("generate_pk_report_container"))))
      
       uiOutput(("generate_pk_report_container"))
      
      )
  )
  
  server <- function(input, output, session) {
    
    source(paste0("~/YANG/global.R"))
    WORKING_HOME = HANDBOOK_HOME
    
    ALL <- reactiveValues(
      DATA   = default_DATA, 
      FIGURE = default_FIGURE, 
      TABLE = default_TABLE,  
      
      cppModel = default_cppModel, 
      ctlModel = default_ctlModel, 
      script = default_script
    )
   
    
    output$generate_pk_report_container <- renderUI({ 
      
      #validate(need(drug_descriptive_analysis_inputData(), message="no data found yet"))
      
      # scripts
      script <- list.files(path=paste0(HANDBOOK_HOME, "/script/"), 
                           full.names = TRUE,
                           pattern="report_template_pk")
      names(script) = basename(script)
      
      # callModule
      ALL = callModule(module_run_report, "generate_pk_report", 
                       ALL, key = "report_template_pk")
      
      #, dataset, script, params=NULL
      
      # UI
      module_run_report_UI(("generate_pk_report"), label = NULL) 
      
    })
      
  }
  
  #shinyApp(ui, server)
  
   

# Run the application 
shinyApp(ui = ui, server = server)

