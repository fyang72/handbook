

# Module UI function
#A module?s UI function should be given a name that is suffixed with Input, 
#Output, or UI; for example, csvFileInput, zoomableChoroplethOutput, or tabOneUI.
#The first argument to a UI function should always be id. This is the namespace for the module. (
#
################################################################################ 
# log in and load data (sidebar)
################################################################################

module_load_data_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #tagList(
    # Custom CSS to hide the default logout panel
    #tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    # The dynamically-generated user panel
    #uiOutput("userpanel"),
    
    # user.name and password
    #textInput(ns("userName"), label="user name:",  value="training", placeholder="xxxxx"),  #"training"
    #passwordInput(ns("password"), label="password:", value="12345", placeholder="xxxxx"),  #"12345"
    #tags$hr(style="border-color: white;"),

    # load observed data  (a whole row)
    fluidRow(
      column(width=6, uiOutput(ns("load_external_data_container"))), 
    
      column(width=6, uiOutput(ns("load_internal_data_container")))
    )#, 
     
    #textInput(ns("data_name"), label="Name it", value="", width="100%", placeholder="naming your file here"),
    
    #actionButton(ns("saveData"), label="Save it", 
    #             style="float:left;color: #fff; background-color: #328332; border-color: #328332"),
    
    
    #fluidRow(column(width=12, tags$hr(style="border-color: white;")))
 # )
}

# Module server functions should be named like their corresponding module UI functions
#Since our UI function was called csvFileInput, we?ll call our server function csvFile:

# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function

################################################################################ 
# log in and load data (sidebar)
################################################################################

module_load_data <- function(input, output, session, ALL, data_name="tdata") {
 
  #print("in load_data::::::")
  
  ns <- session$ns
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
   
  login = NULL
  login$status = FALSE
  login$user.name.lst = c("training", "feng.yang"  ) 

  globalVars <- reactiveValues(login=login)
   
  observeEvent(session, { 
    
    # Load server detail objects into the global workspace.
    globalConfigurationDirectory <- "/data/BiCS_RDE_Development/shiny-server_development/global_configuration/"
    source(file=paste(globalConfigurationDirectory,"load_server_configuration_objects.R",sep=""),local=TRUE)
    
    
    globalVars$login$user.name = determineCurrentUser(session=session)
    globalVars$login$status = globalVars$login$user.name %in% globalVars$login$user.name.lst  
  })
  
   
  # Store the location of the data.
  # dataFolderLocation <- paste0("../../../data_and_analysis_",tolower(x=serverType),"/pharmacometrics/pharmacometrics_data/")
  #dataFolderLocation <- paste0("../../../data_and_analysis_",tolower(x=serverType),"/pharmacometrics/data_repository/")
  #reportTheStatus(statusToDisplay=dataFolderLocation)
  
  dataFolderLocation <- paste0(dirname(getwd()), "/data/")
  
  
  #--------------------------------------  
  # the default logout panel partially obscures the dropdown menu icon. 
  #We can instead add a user panel with dynamic UI (generated on the server) 
  #and hide the default logout panel, as shown below: 
  #--------------------------------------
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    #if (!is.null(session$user)) {
      tagList(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    #}
  })
  
    
  
  #--------------------------------------  
  # load data 
  #--------------------------------------  
  output$load_external_data_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
     
       fileInput(ns("which_external_data"), 
                 label = "Load external data", 
                 width="100%" ) # h5
                    # accept=c('text/csv/sas7bdat', 
                    #          'text/comma-separated-values,text/plain', 
                    #          '.xlsx',
                    #          '.xls',
                    #          '.csv', 
                    #          '.sas7bdat', 
                    #          '.RData'))
  })
   
   
  output$load_internal_data_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    dirs.list = list.files(path = "./data", full.names = FALSE, recursive = FALSE)
    dirs.list = c("", dirs.list)
    selectizeInput(ns("which_internal_data"), 
                   width = '100%',
                   label    = "Load internal data", 
                   choices  = c("", dirs.list), 
                   multiple = FALSE,
                   selected = dirs.list[1]) 
    
  })
  
  
  
  #--------------------------------------  
  # adpx
  #-------------------------------------- 
  load_external_data <- reactive({
    
    validate(need(input$which_external_data, message = FALSE))
    inFile = input$which_external_data
     
    ext <- tools::file_ext(inFile$name)
    #file.rename(inFile$datapath,
    #            paste(inFile$datapath, ext, sep="."))
    
    #file.name = inFile$datapath  # paste(inFile$datapath, ext, sep=".")
    adpx = switch(ext,
            "csv" = read_csv(inFile$datapath, col_names=TRUE )  %>% as.data.frame(),
            "xlsx"=read_excel(inFile$datapath, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
            "xls" = read_excel(inFile$datapath)  %>% as.data.frame(),
            "sas7bdat" =  read_sas(inFile$datapath)  %>% as.data.frame(), 
            "RData" =  load(inFile$datapath)    
    )
 
    if(!is.null(adpx)) {attr(adpx, "file.name") = inFile$name}
    if(!is.null(adpx)) {attr(adpx, "locaton.source") = "external"}
     
    adpx  
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", input$which_external_data$name)
    cat(msg, "\n")
  })
  
   
  
  
  load_internal_data <- reactive({
  
    validate(need(input$which_internal_data, message=FALSE))
    
    inFile = input$which_internal_data
    inFile = paste0("./data/", inFile)
  
    ext <- tools::file_ext(inFile) 
    adpx = switch(ext,
                  "csv" = read_csv(inFile, col_names=TRUE )  %>% as.data.frame(),
                  "xlsx"=read_excel(inFile, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
                  "xls" = read_excel(inFile)  %>% as.data.frame(),
                  "sas7bdat" =  read_sas(inFile)  %>% as.data.frame(), 
                  "RData" =  load(inFile)   # MUST NAMED AS "adpx"   need some work 
    )
    
    if(!is.null(adpx)) {attr(adpx, "file.name") =  inFile}
    if(!is.null(adpx)) {attr(adpx, "locaton.source") = "internal"}
    adpx 
  })
  
  
  
  observeEvent(load_external_data(), {
    validate(need(load_external_data(), message=FALSE))
    ALL$DATA[[data_name]] = load_external_data() #%>% as.data.frame()
  })
  
  observeEvent(load_internal_data(), {
    validate(need(load_internal_data(), message=FALSE))
    ALL$DATA[[data_name]] = load_internal_data() #%>% as.data.frame()
     
  })
  
  

  #add figure to log when action button is pressed
  observeEvent(input$saveData, {
     validate(need(input$data_name, message=FALSE)  
     ) 
     newData = ALL$DATA[[data_name]]
     attributes(newData, "description") = "teststeststsets"
     ALL$DATA[[input$data_name]] <- newData
  
  })
     

  
  return(ALL)
}
