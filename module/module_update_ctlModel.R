
################################################################################ 
# module_load_ctlModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_update_ctlModel_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
  
    fluidRow(
      column(width=12,  
             HTML(colFmt("Please check the consistency between your 'nmdat' and 'ctl' such as $DATA and $INPUT, 
                          You may modify your model at your will and then re-assign a name for it, followed by 
                          re-loading your model from 'within session'.", color="gray")))
    ),
    
    fluidRow(
      column(width=12,   
             uiOutput(ns("ctlModel_content_container")) 
      )
    ) 
  
  )
}

################################################################################ 
# main function: module_load_ctlModel
################################################################################

module_update_ctlModel <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {

ns <- session$ns 
values <- reactiveValues()
 
model_name <- reactive({
  ctlModel_file_name <- basename(attributes(ALL$ctlModel[[ctlModel_name]])$file_name)
  model_name <- tools::file_path_sans_ext(basename(ctlModel_file_name))
})


#--------------------------------------  
# ctlModel_content_container
#--------------------------------------

output$ctlModel_content_container <- renderUI({
  ctlModel =  ALL$ctlModel[[ctlModel_name]]
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(ctlModel, message=FALSE), 
           need(ALL$DATA[[ctlModel_name]], message="please load the nmdat first")
  )
   
   value = ctlModel   
   value = paste0(value, sep="\n")   
   value = paste0(value, collapse="")

  tagList(
    fluidRow( 
      column(12, 
             textInput(ns("nmdat_name"), 
                       value= basename(attributes(ALL$DATA[[ctlModel_name]])$file_name),
                       width="100%",
                       label="$DATA:") 
      )
    ), 
    
    fluidRow( 
      column(12, 
             textInput(ns("nmdat_input"), 
                       value= paste0("$INPUT ", paste0(colnames(ALL$DATA[[ctlModel_name]]),  collapse=" ")), 
                       width="100%",
                       label="$INPUT:") 
      )
    ), 
     
    fluidRow( 
      column(width=3,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("ctlModel_name"), 
                       value= model_name(), 
                       label=NULL, 
                       placeholder = "rename your model here")
             ),
      
      column(3, 
             actionButton(ns("update_model"), label="Save model", style=actionButton_style )
      )
    ),
    
    fluidRow(
      column(12, 
             aceEditor(ns("ctlModel_content"), 
                       mode="fortran", value=value, 
                       theme = "crimson_editor",   # chrome
                       autoComplete = "enabled",
                       height = "1000px", 
                       fontSize = 15 
             )
      )
    )
  )
  
})



#--------------------
# ctlModel_content
#--------------------
observeEvent(input$update_model, {
  ctlModel = input$ctlModel_content
  validate(need(ctlModel, message="no ctlModel found..."), 
           need(input$ctlModel_name==gsub(" ", "", input$ctlModel_name, fix=TRUE), 
                message="no empty space in your file name")
  )
     
  attr(ctlModel, 'file_name') <- input$ctlModel_name
  attr(ctlModel, 'locaton_source') <- "session"
  
  ALL$ctlModel[[input$ctlModel_name]]  = ctlModel
  
  showNotification("model was updated", type="message") # "default, "message", "warning", "error"
})

return(ALL)
}