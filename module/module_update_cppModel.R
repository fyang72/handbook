
################################################################################ 
# module_load_ctlModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_update_cppModel_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(width=12,   
           uiOutput(ns("cppModelContent_container")) 
    )
  ) 
  
}

################################################################################ 
# main function: module_load_ctlModel
################################################################################

module_update_cppModel <- function(input, output, session, ALL, cppModel_name="cppModel_name") {
  
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  #--------------------------------------  
  # cppModelContent_container
  #--------------------------------------
 
output$cppModelContent_container <- renderUI({
  cppModel =  ALL$cppModel[[cppModel_name]]
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(cppModel, message=FALSE)
  )
  
  #value = readLines(cppFile)
  value = see(cppModel,raw=TRUE)
  value = paste0(value, sep="\n")   # sep="<br/>")
  value = paste0(value, collapse="")
  #value = gsub("\n", '<br/>', value, fixed=TRUE)
  
  # HTML(value) 
  tagList(
    fluidRow(
      column(width=12, "You may modify the loaded model and then re-assign a name for it.")), 
    fluidRow(
      #column(width=2, "model name"),
      column(width=4,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("model_name"), value=NULL, placeholder ="cppModel-name", label=NULL, width="100%")),
      
      column(6, 
             textInput(ns("read_cppModel_message"), 
                       width="100%",
                       label= NULL, 
                       placeholder = "read cppModel sucessfully/failed?"
             )
      ),
       
      column(2, 
             actionButton(ns("update_model"), label="Save model", style=actionButton.style )
      )
    ),
    fluidRow(
      column(12,
             textAreaInput(ns("cppModelContent"), label=NULL, value=value, rows=200,
                           width = '785px',   #400px', or '100%'
                           placeholder= "Your cppModel here.") 
      )
    )
  )
  
})



#--------------------
# cppModelContent
#--------------------
observeEvent(input$update_model, {
  
  mymodel =input$cppModelContent
  validate(need(mymodel, message="Empty cppModel..."))
  
  # try to read model after editing 
  cppModel =  tryCatch(mread("cppModel", tempdir(), mymodel),     # ?mread , ?mcode, 
                       error=function(e) {
                         print("mread cppModel not sucessful..."); 
                         return(NULL)
                       } #, finally = {
                       # eval(parse(text=txt)) %>% as.data.frame()
                       #}
  )
  
  if (is.function(cppModel)) {cppModel=NULL}  # avoid key words such as expand
  #validate(need(cppModel, message="Load cppModel not sucefully"))
   
  if(is.null(cppModel)) {updateTextInput(session, "read_cppModel_message", value="error found in cppModel")}
  validate(need(cppModel, message="error found in cppModel"))
  
  print("update cppModel sucessfully")
  updateTextInput(session, "read_cppModel_message", value="read cppModel successfully")
  
  ALL$cppModel[[input$model_name]]  = cppModel
  
})
  
  return(ALL)
}