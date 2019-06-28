
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_script_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(6, uiOutput(ns("script_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12, uiOutput(ns("script_container"))))
  )
  
} 

# Module server function
module_review_script <- function(input, output, session, ALL) {
  
  ns <- session$ns
  
  # script_selector
  output$script_selector <- renderUI({  
    # name_lst=list.files(path = paste0(HOME, "/ctl"), 
    #                     full.names = FALSE, 
    #                     recursive = FALSE, 
    #                     pattern=".ctl", 
    #                     include.dirs=FALSE
    # )  
    # 
    # 
    name_lst <- c("", names(ALL$script)) %>% unique()
    selectInput(ns("select_script"), label = "Select script:",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  
  ## use renderUI to display table
  output$script_container <- renderUI({
    
    validate(need(input$select_script, message=FALSE))
    
    script_content = ALL$script[[input$select_script]]
    # readLines(paste0(HOME, "/ctl/", input$select_script))
    # 
    
    tagList(
      fluidRow(
        column(width=12, 
               HTML(colFmt("You may modify the loaded script and then re-assign a name for it.", color="gray")))
      ), 
      
      fluidRow(
        column(6,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
               textInput(ns("script_name"), value=NULL, 
                         placeholder ="script-name", label=NULL, width="100%")
        ),
        
        column(2, 
               actionButton(ns("save_script"), label="Save script", style=actionButton_style )
        ),
        
        column(2,  
               actionButton(ns("delete_script"),label="Delete script", 
                            style=actionButton_style))
      ),
      
      aceEditor(ns("script_content"), 
                mode="c_cpp", 
                value=paste0(script_content, collapse="\n"), 
                theme = "crimson_editor",   # chrome
                autoComplete = "enabled",
                height = "1000px", 
                fontSize = 15 
      )
      
    )
  })
  
  
  #--------------------
  # event for save_script
  #--------------------
  observeEvent(input$save_script, {
    
    script = input$script_content
    validate(need(script, message="Empty script..."), 
             need(input$script_name, message="no script name")
    )
    
    ALL$script[[input$script_name]]  = script
    # "default, "message", "warning", "error" 
    showNotification("script saved", type="message")  # "default, "message", "warning", "error"  
    
    
  })
  
  #--------------------
  # event for delete_script
  #--------------------
  observeEvent(input$delete_script, {
    
    validate(need(input$select_script, message="delete_script..."))
    ALL$script[[input$select_script]]  = NULL 
    
    showNotification("script deleted", type="message")  # "default, "message", "warning", "error"  
  })
  
}