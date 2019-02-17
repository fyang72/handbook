

# Module UI function
#A module?s UI function should be given a name that is suffixed with Input, 
#Output, or UI; for example, csvFileInput, zoomableChoroplethOutput, or tabOneUI.
#The first argument to a UI function should always be id. This is the namespace for the module. (
#
################################################################################ 
# log in and load data (sidebar)
################################################################################

module_load_ctlModel_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #tagList( 
    fluidRow(
      column(width=6, uiOutput(ns("load_external_ctlModel_container"))), 
      
      column(width=6, uiOutput(ns("load_internal_ctlModel_container")))
    ) 
 # )
}

################################################################################ 
# log in and load data (sidebar)
################################################################################

module_load_ctlModel <- function(input, output, session, ALL, ctlModel_name="ctlModel") {
  
  #print("in load_ctlModel::::::")
  
  ns <- session$ns
  
  #--------------------------------------  
  # load data 
  #--------------------------------------  
  output$load_external_ctlModel_container <- renderUI({
    #validate(need(globalVars$login$status, message=FALSE))
    
    fileInput(ns("which_external_ctlModel"), 
              label = "Load external ctlModel", width="100%" ) # h5
    # accept=c('text/csv/sas7bdat', 
    #          'text/comma-separated-values,text/plain', 
    #          '.xlsx',
    #          '.xls',
    #          '.csv', 
    #          '.sas7bdat', 
    #          '.RData'))
  })
  
  
  output$load_internal_ctlModel_container <- renderUI({
    #validate(need(globalVars$login$status, message=FALSE))
    
    
    dirs.list = list.files(path = paste0("./model/ctl"), full.names = FALSE, recursive = FALSE)
    print(dirs.list)
    dirs.list = c("", dirs.list)
    selectizeInput(ns("which_internal_ctlModel"), 
                   width = '100%',
                   label    = "Load internal ctlModel", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   selected = dirs.list[1]) 
    
  })
  
  
  
  #--------------------------------------  
  # adpx
  #-------------------------------------- 
  load_external_ctlModel <- reactive({
    validate(need(input$which_external_ctlModel, message = FALSE))
    
    inFile = input$which_external_ctlModel
    
    ext <- tools::file_ext(inFile$name)
    #file.rename(inFile$datapath,
    #            paste(inFile$datapath, ext, sep="."))
    
    #source(paste(inFile$datapath, ext, sep="."))
    #ctlModel <- mread("SHINY", tempdir(), mymodel)     # ?mread , ?mcode
    ctlModel.file = inFile$datapath  #paste(inFile$datapath, ext, sep=".")
    
    #readLines("ex.data", n = -1)
    ctlModel = readLines(ctlModel.file)   # "./model/ctl/control5.ctl")
    #unlink(ctlModel.file) # tidy up
      
    attr(ctlModel, 'file.name') <- inFile$name
    attr(ctlModel, 'locaton.source') <- "external"
    ctlModel
  })
  
  
  load_internal_ctlModel <- reactive({
    validate(need(input$which_internal_ctlModel, message=FALSE))
     
    
    ctlModel.file = paste0("./model/ctl/", input$which_internal_ctlModel)
 
    ctlModel = readLines(ctlModel.file)   # "./model/ctl/control5.ctl")

    attr(ctlModel, 'file.name') <- ctlModel.file
    attr(ctlModel, 'locaton.source') <- "internal"
    ctlModel
  })
  
  
  
  observeEvent(load_external_ctlModel(), {
    validate(need(load_external_ctlModel(), message=FALSE))
    ALL$ctlModel[[ctlModel_name]] = load_external_ctlModel() #%>% as.data.frame()
  })
  
  observeEvent(load_internal_ctlModel(), {
    validate(need(load_internal_ctlModel(), message=FALSE))
    ALL$ctlModel[[ctlModel_name]] = load_internal_ctlModel() #%>% as.data.frame()
    
  })
  
  
  
  
  return(ALL)
}
