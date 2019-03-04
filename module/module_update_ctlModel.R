
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
             uiOutput(ns("ctlModelContent_container")) 
      )
    ) 
  
  )
}

################################################################################ 
# main function: module_load_ctlModel
################################################################################

module_update_ctlModel <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {
  
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  #--------------------------------------  
  # ctlModelContent_container
  #--------------------------------------
  
  output$ctlModelContent_container <- renderUI({
    ctlModel =  ALL$ctlModel[[ctlModel_name]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(ctlModel, message=FALSE), 
             need(ALL$DATA[[ctlModel_name]], message="please load the nmdat first")
    )
    
    #value = readLines(cppFile)
     value = ctlModel   # see(ctlModel,raw=TRUE)
     value = paste0(value, sep="\n")   # sep="<br/>")
     value = paste0(value, collapse="")
    #value = gsub("\n", '<br/>', value, fixed=TRUE)
        # HTML(value) 
     
    tagList(
      
      fluidRow(
        #column(2, "$DATA:"),
        column(12, 
               textInput(ns("nmdat_name"), 
                         value= basename(attributes(ALL$DATA[[ctlModel_name]])$file.name),
                         width="100%",
                         label="$DATA:") 
        )
      ), 
      
      fluidRow(
        #column(2, "$INPUT:"),
        column(12, 
               textInput(ns("nmdat_input"), 
                         value= paste0("$INPUT ", paste0(colnames(ALL$DATA[[ctlModel_name]]),  collapse=" ")), 
                         width="100%",
                         label="$INPUT:") 
        )
      ), 
      
      fluidRow(
        #column(width=3, h5("Name this model")), # offset=3, 
        column(width=3,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
               textInput(ns("ctlModel_name"), value=NULL, label=NULL, placeholder = "rename your model here")
               ),
        
        column(3, 
               actionButton(ns("update_model"), label="Save model", style=actionButton.style )
        )
      ),
      
      fluidRow(
        column(12,
               textAreaInput(ns("ctlModelContent"), label=NULL, value=value, rows=200,
                             #width="100%",
                             width = '785px', #   '800px',   #400px', or '100%'
                             placeholder= "Your ctlModel here."
                             ) 
        )
      )
    )
    
  })
  
  
  
  #--------------------
  # ctlModelContent
  #--------------------
  observeEvent(input$update_model, {
    ctlModel = input$ctlModelContent
    validate(need(ctlModel, message="no ctlModel found..."), 
             need(input$ctlModel_name==gsub(" ", "", input$ctlModel_name, fix=TRUE), 
                  message="no empty space in your file name")
    )
     
    print("Update model sucessfully")
    ALL$ctlModel[[input$ctlModel_name]]  = ctlModel
    
  })
  
  return(ALL)
}