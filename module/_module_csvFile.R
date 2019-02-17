

# Module UI function
#A module?s UI function should be given a name that is suffixed with Input, 
#Output, or UI; for example, csvFileInput, zoomableChoroplethOutput, or tabOneUI.
#The first argument to a UI function should always be id. This is the namespace for the module. (
#
################################################################################ 
# log in and load data (sidebar)
################################################################################

csvFileInput <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    
    # user.name and password
    #textInput(ns("userName"), label="user name:",  value="training", placeholder="xxxxx"),  #"training"
    #passwordInput(ns("password"), label="password:", value="12345", placeholder="xxxxx"),  #"12345"
    #tags$hr(style="border-color: white;"),

    # load observed data  (a whole row)
    fluidRow(column(width=12, uiOutput(ns("load_rawData")))), 
    
    fluidRow(column(width=12, uiOutput(ns("load_testData")))), 
     
    textInput(ns("data_name"), label="Name it", value="", width="100%", placeholder="naming your file here"),
    
    actionButton(ns("saveData"), label="Save it", style=actionButton.style),
    
    
    fluidRow(column(width=12, tags$hr(style="border-color: white;")))
  )
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

csvFile <- function(input, output, session, DATA) {
  
  
  print("in csvFile::::::")
  
  ns <- session$ns
  
  # Store the location of the data.
   #dataFolderLocation <- paste0("../../../data_and_analysis_",tolower(x=serverType),"/pharmacometrics/pharmacometrics_data/")
  dataFolderLocation <- paste0("../../../data_and_analysis_",tolower(x=serverType),"/pharmacometrics/data_repository/")
  #reportTheStatus(statusToDisplay=dataFolderLocation)
  
  
  #dataFolderLocation <- paste0(dirname(getwd()), "/data/")
  
  # only for internal use
  values <- reactiveValues(adpx=NULL)
  
  
  #--------------------------------------  
  # the default logout panel partially obscures the dropdown menu icon. 
  #We can instead add a user panel with dynamic UI (generated on the server) 
  #and hide the default logout panel, as shown below: 
  #--------------------------------------
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  
  
  #--------------------------------------  
  # login 
  #--------------------------------------
  # observeEvent({
  #   input$userName
  #   input$password
  # }, {
  #   
  #   validate(need(DATA, message=FALSE), 
  #            need(input$userName, message=FALSE),
  #            need(input$password, message=FALSE) 
  #            )
  # 
  #   globalVars$login$status = login(input$userName, input$password)
  #     
  #   if (globalVars$login$status==TRUE) {
  #     globalVars$login$userName =  input$userName
  #     globalVars$login$password =  input$password
  #   }
  # })
    
  
  #--------------------------------------  
  # load data 
  #--------------------------------------  
  output$load_rawData <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    #tagList(
      #tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))), 
 
       fileInput(ns("adpx_loading"), label = ("Load observed data"), width="100%" , # h5
                    accept=c('text/csv/sas7bdat', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx',
                             '.xls',
                             '.csv', 
                             '.sas7bdat', 
                             '.RData'))
   # )
    
    
  })
   
   
  output$load_testData <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    dirs.list = list.files(path = "./data", full.names = FALSE, recursive = FALSE)
    dirs.list = c("", dirs.list)
    selectizeInput(ns("which_testData"), 
                   label    = "Select testData" , 
                   choices  = c("", dirs.list), 
                   multiple = FALSE,
                   selected = dirs.list[1]) 
    
  })
  
  
  
  #--------------------------------------  
  # adpx
  #--------------------------------------
  # The user's data, parsed into a data frame
  adpx <- reactive({
    
    validate(need(input$adpx_loading, message = FALSE))
    inFile = input$adpx_loading
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    
    #print(paste(inFile$datapath, ext, sep="."))
    cat(file=stderr(), "##############Step: loading adpx into the session#################", "\n")
    
    adpx = switch(ext,
            "csv" = read_csv(paste(inFile$datapath, ext, sep="."), col_names=TRUE )  %>% as.data.frame(),
            "xlsx"=read_excel(paste(inFile$datapath, ext, sep="."), sheet = 1, col_names = TRUE)  %>% as.data.frame(),
            "xls" = read_excel(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame(),
            "sas7bdat" =  read_sas(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame(), 
            "RData" =  load(paste(inFile$datapath, ext, sep="."))   # MUST NAMED AS "adpx"   need some work 
    )
    
    # not emtpy space in column names, not allowed.
    colnames(adpx) = gsub(" ", ".",  colnames(adpx), fixed=TRUE)
    colnames(adpx) = toupper(colnames(adpx))
    
    adpx
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", input$adpx_loading$name)
    cat(msg, "\n")
  })
  
   
  
  
  testData <- reactive({
  
    validate(need(input$which_testData, message=FALSE))
    
    adpx =   read_csv(paste0("./data/", input$which_testData))  %>% as.data.frame()
                #"xlsx"=read_excel(paste(inFile$datapath, ext, sep="."), sheet = 1, col_names = TRUE)  %>% as.data.frame(),
                #"xls" = read_excel(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame(),
                #"sas7bdat" =  read_sas(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame()
 
    colnames(adpx) = gsub(" ", ".", colnames(adpx), fix=TRUE)
    colnames(adpx) = toupper(colnames(adpx))
    
   adpx
  
  })
  
  
  
  observeEvent(adpx(), {
    validate(need(adpx(), message=FALSE))
    
    values[["adpx"]] = adpx()
    
  })
  
  observeEvent(testData(), {
    validate(need(testData(), message=FALSE))
    
    values[["adpx"]] = testData()
    
  })
  
  

    #add figure to log when action button is pressed
    observeEvent(input$saveData, {
      validate(need(input$data_name, message=FALSE), 
               need( values[["adpx"]], message=FALSE)  
      ) 
      
      newData <- isolate(values[["adpx"]] )
      data_name = isolate(input$data_name)
      DATA = addNewData(newData, data_name, DATA)
      
     # newData <- list(isolate(adpx()))
     # data_name <- isolate(input$data_name)
     # names(newData) = data_name
      
      
      # CurrentLog_mDATA   <- isolate(DATA$mDATA)
      # DATA$mDATA[[data_name]] <- newData[[data_name]]
      # 
      # 
      # if (data_name %in% names(CurrentLog_mDATA)) {
      #     cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
      #  
      # }
      
    })
     
    
  return(DATA)
}
