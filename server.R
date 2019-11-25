
# 3) use R2222 as a real example to process adsl, adex, adpc, cross-check these datasets
# 4) use R1193/1500 as a real example for PKPD analsis
# 5) use R2810 as a real example for simulation ananlysis
# 6) optimize reactive, use if NULL return(NULL)

# 9) Optimize the layout of check out, 
#11) converType is not working yet.

# boxplot, bookmark, build_adex, download data, debug/cleanup for the current version.
# begin to code R2810 PK data and modeling results (learn PsN on cluster)


# target for Nov EARL Boston 2017 meeting
# 0) whey there are double line in simulations

# 1) basic checkin , build_adpx, adex, adsl, etc.
# 2) basic smart learning dataset structure
# 3) include dynamic filter in all module
# 4) basic PKPD plot, sigmoid curvefit in scatter plot
# 5) Think about how to refresh the whole report, bookmark, refresh a report
# 6) prepare the talk (30 mins)


####################### #########################################################
# server
################################################################################

#### Load server detail objects into the global workspace. ####
#globalConfigurationDirectory <- "../../global_configuration/"
#source(file=paste(globalConfigurationDirectory,"load_server_configuration_objects.R",sep=""),local=TRUE)


shinyServer(function(input, output, session) {
  
  # #### Usage logger iframe. ####
  # source(
  #   file=paste(
  #     "../",
  #     relativePathToRootDirectoryForUsageLoggerViewerInjectionScripts,
  #     "usage_logger_code_injection_for_server.R",
  #     sep=""
  #   ),
  #   local=TRUE
  # )
  
  output$notificationMenu <- renderMenu({
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = paste0(length(DATA$mDATA), " of datasets"),
                   icon("database", lib = "font-awesome")
                 ),
                 notificationItem(
                   text = paste0(length(FIGURE_ALL$mFIGURES), " of figures"),
                   icon("bar-chart-o", lib = "font-awesome") #status = "success"
                 ),
                 notificationItem(
                   text = paste0(length(TABLE_ALL$mTABLES), " of tables"),
                   icon("table", lib = "font-awesome")    # status = "warning"
                 )
    )
  })
  
  
  #https://fontawesome.com/icons?d=gallery
  #https://fontawesome.com/v4.7.0/icons/
  output$messageMenu <- renderMenu({
    messageData = data.frame(type=c("dataset","figure","table"), 
                             number=c(length(DATA$mDATA), length(FIGURE_ALL$mFIGURES), length(TABLE_ALL$mTABLES)), 
                             icon = c("database", "bar-chart-o", "table" )
    )
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["type"]], message = row[["number"]], 
                  icon=icon(row[["icon"]], lib = "font-awesome") ) # fa-bar-chart  picture-o
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", badgeStatus = "primary", .list = msgs)
  })
  
  
  
  
  output$taskMenu <- renderMenu({
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
    
  })
  
  
  #######################################
  # check in data and manipulation
  #######################################
  
  # maintain a list of FlexTable object such that we can print and modify them later
  observeEvent(session, { 
    globalVars$login$user.name = "feng.yang"   # determineCurrentUser(session=session)
    print("current users:")
    print(globalVars$login$user.name)
    
    globalVars$login$status = globalVars$login$user.name %in% globalVars$login$user.name.lst  
  })
  
  
  DATA <- reactiveValues(
    mDATA   = list(),
    mTITLES = list(), 
    NumOfDataset = 0 
    
  )
  
  
  # load raw data
  DATA <- callModule(csvFile, "myinputData", DATA)
  #callModule(rawDataTab, "myrawDataTab", rawData)  # render the rawData, no change made
  
  # gernated adpx from adsl, adex, adpc, however, substitute by rawData$adpx() if provided. 
  DATA <- callModule(checkInCols, "mycheckInCols",  DATA)
  
  # gernated adpx from adsl, adex, adpc, however, substitute by rawData$adpx() if provided. 
  DATA <- callModule(checkInRows, "mycheckInRows",  DATA)
  
  DATA <- callModule(deriveData, "myderiveData",  DATA)
  
  # return simulated dataset
  DATA <- callModule(simulation, "mysimulation", DATA)
  
  # return derived pkTab from simulated dataset
  DATA <- callModule(deriveTab, "myderiveTab", DATA)   # derive PK 
  
  
  
  #######################################
  # ordering figure  
  #######################################
  
  # maintain a list of FlexTable object such that we can print and modify them later
  FIGURE_ALL <- reactiveValues(
    mFIGURES = list(),
    mTITLES  = list(), 
    mFOOTNOTES = list(), 
    NumOfFig   = 0
  )
  
  # maintain a list of FlexTable object such that we can print and modify them later
  TABLE_ALL <- reactiveValues(
    mTABLES    = list(),
    mTITLES = list(), 
    mFOOTNOTES = list(), 
    NumOfTab = 0
  )
  
  # only return FIGURE_ALL
  #FIGURE_ALL <- callModule(diagnostic, "mydiagnostic", DATA, FIGURE_ALL)
  
  
  # only return FIGURE_ALL
  FIGURE_ALL <- callModule(xyplot, "myxyplot", DATA, FIGURE_ALL)
  
  
  # will return FIGURE_ALL, TABLE_ALL
  FIGURE_ALL <- callModule(boxplot, "myboxplot", DATA, FIGURE_ALL)
  
  # will return FIGURE_ALL, TABLE_ALL
  tt <- callModule(barplot, "mybarplot", DATA, TABLE_ALL, FIGURE_ALL)
  FIGURE_ALL = tt$FIGURE_ALL
  TABLE_ALL  = tt$TABLE_ALL
  
  
  # only return FIGURE_ALL
  #TABLE_ALL <- callModule(summaryTab, "mysummaryTab", DATA, TABLE_ALL)
  
  # return derived pkTab from simulated dataset
  TABLE_ALL <- callModule(dataManipulation, "mydataManipulation", DATA, TABLE_ALL)
  
  
  #######################################
  # review order
  #######################################
  callModule(figReview, "myfigReview",  DATA, FIGURE_ALL)
  
  callModule(tabReview, "mytabReview",  DATA, TABLE_ALL)
  
  callModule(datasetReview, "mydatasetReview",  DATA, TABLE_ALL)
  
  callModule(modelReview, "mymodelReview")
  
  #######################################
  # check out
  #######################################  
  callModule(checkOut, "mycheckOut",  DATA, FIGURE_ALL, TABLE_ALL)
  
  
  #######################################
  # scripte note
  #######################################  
  callModule(scriptLib, "myscriptLib")
  
  
  
  # session$user
  #  print("session$user")
  # print(session$user)
  #  users_data <- data.frame(USERS = session$user, START = Sys.time())
  session$onSessionEnded(function() {
    
    isolate({
      write_csv(globalVars$magicTab, path="./lib/magicTab.csv")
      write_csv(globalVars$scriptTab, path="./lib/scriptTab.csv")
      
    })
  })  
  
  
  
  
  
})





################################################################################
# run it
################################################################################


#shinyApp(ui, server)



