
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_ctlModel_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(6, uiOutput(ns("ctlModel_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12, uiOutput(ns("ctlModel_container"))))
  )
  
} 

# Module server function
module_review_ctlModel <- function(input, output, session, ALL) {
  
  ns <- session$ns
  
  # ctlModel_selector
  output$ctlModel_selector <- renderUI({  
    # name_lst=list.files(path = paste0(HOME, "/ctl"), 
    #                     full.names = FALSE, 
    #                     recursive = FALSE, 
    #                     pattern=".ctl", 
    #                     include.dirs=FALSE
    # )  
    # 
    # 
    name_lst <- c("", names(ALL$ctlModel)) %>% unique()
    selectInput(ns("select_ctlModel"), label = "Select ctlModel:",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  
  ## use renderUI to display table
  output$ctlModel_container <- renderUI({
    
    validate(need(input$select_ctlModel, message=FALSE))
    
    script_content = ALL$ctlModel[[input$select_ctlModel]]
    # readLines(paste0(HOME, "/ctl/", input$select_ctlModel))
    # 
    
    tagList(
      fluidRow(
        column(width=12, 
               HTML(colFmt("You may modify the loaded model and then re-assign a name for it.", color="gray")))
      ), 
      
      fluidRow(
        column(6,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
               textInput(ns("model_name"), value=NULL, 
                         placeholder ="ctlModel-name", label=NULL, width="100%")
        ),
        
        column(2, 
               actionButton(ns("save_model"), label="Save model", style=actionButton_style )
        ),
        
        column(2,  
               actionButton(ns("delete_model"),label="Delete model", 
                            style=actionButton_style))
      ),
      
      aceEditor(ns("ctlModel_content"), 
                mode="fortran", 
                value=paste0(script_content, collapse="\n"), 
                theme = "crimson_editor",   # chrome
                autoComplete = "enabled",
                height = "1000px", 
                fontSize = 15 
      )
      
    )
  })
  
  
  #--------------------
  # event for save_model
  #--------------------
  observeEvent(input$save_model, {
    
    ctlModel = input$ctlModel_content
    validate(need(ctlModel, message="Empty ctlModel..."), 
             need(input$model_name, message="no model name")
    )
    
    ALL$ctlModel[[input$model_name]]  = ctlModel
    # "default, "message", "warning", "error" 
    showNotification("ctlModel saved", type="message")  # "default, "message", "warning", "error"  
    
    
  })
  
  #--------------------
  # event for delete_model
  #--------------------
  observeEvent(input$delete_model, {
    
    validate(need(input$select_ctlModel, message="delete_model..."))
    ALL$ctlModel[[input$select_ctlModel]]  = NULL 
    
    showNotification("ctlModel deleted", type="message")  # "default, "message", "warning", "error"  
  })
  
}