

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

scriptLibUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Control panel:", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 fluidRow(column(12,
                   radioButtons(ns("search_or_add"), label=NULL, #"Select cpp model from:", 
                              choices=c("search library", "add script"), inline=TRUE), 
                   tags$hr(style="border-color: black;"))),
                 
                 fluidRow(
                   column(6, 
                        uiOutput(ns("domain_selector"))    # all, public, or individual
                        #uiOutput(ns("domain2_selector"))     # all, public, or individual
                        
                        
                        # tags$hr(style="border-color: black;")
                 ), 
                   column(6,
                        # x-variable, y-variable, and group_by
                        uiOutput(ns("category_selector"))  # simulation, parse, dplyr, ggplot
                        #uiOutput(ns("category2_selector"))
                         
                        #tags$hr(style="border-color: black;")
                        
                 )), 
                 fluidRow(column(12, uiOutput(ns("key_selector")))),
                 #fluidRow(column(12,uiOutput(ns("key2_selector")))),
                 
                 fluidRow(column(12,uiOutput(ns("dates_selector")))) 
                 #fluidRow(column(12,uiOutput(ns("dates2_selector"))))
                 ) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Script cheatsheet", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 fluidRow(
                   column(12,
                          fluidRow(
                            column(width=4,  
                                   #actionButton(ns("runSimulation"), label="Run simulation", style=actionButton.style ) 
                                   uiOutput(ns("scriptID_selector")),
                                   uiOutput(ns("scriptID2_selector"))),
                            
                            column(width=1, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                                   uiOutput(ns("prevScript_selector"))), 
                                   
                            
                            column(width=1, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                                   uiOutput(ns("nextScript_selector"))), 
                             
                            column(width=1, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                                   uiOutput(ns("saveScript_selector")))
                          ), 
                          
                          fluidRow(
                            column(width=12, uiOutput(ns("script_container")))
                          )
                   ))
                  
                 

             )
           )
    )
  )
  
  
}

# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function
scriptLib <- function(input, output, session) {
  
  ns <- session$ns
   
  values <- reactiveValues(iScript=1)
  
   
  
  
  cat.lst = c("all", "event", "parse", "ggplot", "datatable")
  
  domain.lst = c("all", "public", "private")
  
  #-----------------------------------------
  # on UI side for search library
  #-----------------------------------------
  
  output$domain_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE) )
             
    #domain.lst = unique(data$DOMAIN)
    radioButtons(ns("domain"), label="Domain", 
                       choices=domain.lst, selected=domain.lst[1], 
                       inline=FALSE)
    
  })
  
   
  
  output$category_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE))
    
    #cat.lst = unique(data$CATEGORY)
    radioButtons(ns("category"), label="Category", 
                       choices=cat.lst, selected=cat.lst[1], 
                       inline=FALSE)
    
  })
  
  
  output$dates_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE))
    
    today = as.Date(as.POSIXlt(Sys.time(), tz="America/New_York"), format = "yyyy-mm-dd")
    
    start = max(min(scriptTab$DATE), today - 365)
    end = today
    
    min = min(scriptTab$DATE)
    max = today  
     
    dateRangeInput(ns("dates"), label = "Date range",
                   start = start, end = end,
                   min = min, max = max)  # input$dates

  })
  
  
  output$key_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE) )
    
    textInput(ns("keys"), label = "Search keys")  # input$dates
    
  })
  
  output$scriptID_selector <- renderUI({
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE), 
             need(input$search_or_add =="search library", message=FALSE)
    )
    
    selectizeInput(ns("scriptID"), label = NULL, # "Select ID", 
                   choices=c("",unique(scriptTab$ID)), selected = "", multiple=FALSE)  # input$dates
    
  })
  
  
  output$nextScript_selector <- renderUI({
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE), 
             need(input$search_or_add =="search library", message=FALSE)
    )
    
    actionButton(ns("nextScript"), label="Next", style=actionButton.style)
    
  })
  
  output$prevScript_selector <- renderUI({
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE), 
             need(input$search_or_add =="search library", message=FALSE)
    )
    
    actionButton(ns("prevScript"), label="Prev", style=actionButton.style)
    
  })
  
  
  #-----------------------------------------
  # on UI side for add script to library
  #-----------------------------------------
  # 
  # output$domain2_selector <- renderUI({
  #   scriptTab = inputData()
  #   validate(need(scriptTab, message=FALSE), 
  #            need(input$search_or_add =="add script", message=FALSE)
  #   )
  #   
  #   domain.lst = setdiff(domain.lst, "all") # unique(data$DOMAIN)
  #   checkboxGroupInput(ns("domain2"), label="Domain", 
  #                      choices=domain.lst, selected=domain.lst[1], 
  #                      inline=FALSE)
  #   
  # })
  # 
  # 
  # 
  # output$category2_selector <- renderUI({
  #   scriptTab = inputData()
  #   validate(need(scriptTab, message=FALSE), 
  #            need(input$search_or_add =="add script", message=FALSE)
  #   )
  #   
  #   cat.lst = setdiff(cat.lst, "all")  #unique(data$CATEGORY)
  #   checkboxGroupInput(ns("category2"), label="Category", 
  #                      choices=cat.lst, selected=cat.lst[1], 
  #                      inline=FALSE)
  #   
  # })
  # 
  #  
  # output$dates2_selector <- renderUI({
  #   scriptTab = inputData()
  #   validate(need(scriptTab, message=FALSE), 
  #            need(input$search_or_add =="add script", message=FALSE)
  #   )
  #   
  #   today = as.Date(as.POSIXlt(Sys.time(), tz="America/New_York"), format = "yyyy-mm-dd")
  #   
  #   start = max(min(scriptTab$DATE), today - 365)
  #   end = today
  #   
  #   min = min(scriptTab$DATE)
  #   max = today  
  #   
  #   dateRangeInput(ns("dates"), label = "Date range",
  #                  start = start, end = end,
  #                  min = min, max = max)  # input$dates
  #   
  # })
  #  
  #  
  # output$key2_selector <- renderUI({
  #   scriptTab = inputData()
  #   validate(need(scriptTab, message=FALSE), 
  #            need(input$search_or_add =="add script", message=FALSE)
  #   )
  #   
  #   textInput(ns("keys2"), label = "Input search keys")  # input$dates
  #   
  # })
  # 
  
  output$scriptID2_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE), 
             need(input$search_or_add =="add script", message=FALSE)
    )
    
    textInput(ns("scriptID2"), label = NULL)  # input$dates
    
  })
  
  output$saveScript_selector <- renderUI({
    scriptTab = inputData()
    validate(need(scriptTab, message=FALSE)
    )
    
    actionButton(ns("saveScript"), label="Save it", style=actionButton.style)
   
  })
  
  
  
  
  #------------------------- 
  #  inputData
  #------------------------- 
  inputData <- reactive({
   
    scriptTab = globalVars$scriptTab
     
    
    validate(need(scriptTab, message=FALSE))
    
    scriptTab
    
  })

  
  #------------------------- 
  #  filterData
  #------------------------- 
  filterData <- reactive({
    validate(need(input$category, message=FALSE), 
             need(input$dates, message=FALSE),
             need(input$domain, message=FALSE)
             )
    scriptTab = inputData()
    scriptTab = scriptTab %>% filter(DATE>=input$dates[1] & DATE<=input$dates[2]
    )
     
    # if (input$scriptID!="") {scriptTab = scriptTab %>% filter(ID %in% input$scriptID)}
    
    if (input$category != "all") {scriptTab = scriptTab %>% filter(CATEGORY %in% input$category)}
    
    if (input$domain == "public") {scriptTab = scriptTab %>% filter(DOMAIN=="public")}
    if (input$domain == "private") {scriptTab = scriptTab %>% 
                                    filter(DOMAIN=="private", USER.NAME==  globalVars$login$userName)}
    
  
    scriptTab
    
  })
  
  
  
  #-----------------------------------------
  ## after align, display the aligned data
  #-----------------------------------------  
  
  observeEvent(filterData(), {
    
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE))
    
    updateSelectizeInput(session, "scriptID", selected=scriptTab$ID[1])
    
  })
  
  # restart
  observeEvent(input$scriptID, {
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE), 
             need(input$scriptID, message=FALSE)
    )
    
    values[["iScript"]] =  which(scriptTab$ID == input$scriptID )
    
  })
  
  
 observeEvent(input$nextScript, {
   scriptTab = filterData()
   
   values[["iScript"]] =values[["iScript"]] + 1
   values[["iScript"]] = min(values[["iScript"]],nrow(scriptTab))
   i = values[["iScript"]]  
   
   updateSelectizeInput(session, "scriptID", selected=scriptTab$ID[i])
 
 })
  
 observeEvent(input$prevScript, {
   scriptTab = filterData()
   
   values[["iScript"]] =  values[["iScript"]] - 1
   values[["iScript"]] = max(1, values[["iScript"]])
   i = values[["iScript"]]
   
   updateSelectizeInput(session, "scriptID", selected=scriptTab$ID[i])
 
 })
 
 observeEvent(input$search_or_add, {
 
   if (input$search_or_add =="add script") { 
     updateTextAreaInput(session, "scriptArea", value="add your script here.")
     updateTextInput(session, "scriptID2", value="type in script ID")
   }
   
   if (input$search_or_add =="search library") { 
     scriptTab = filterData()
     validate(need(scriptTab, message=FALSE))
     
     updateSelectizeInput(session, "scriptID", selected=values[["iScript"]])
     
     i = values[["iScript"]]
     value =  scriptTab$VALUE[i]
     updateTextAreaInput(session, "scriptArea",  value= value)
     
   }
   
 })

 
 
 observeEvent(input$saveScript, {
   scriptTab = inputData()
   validate(need(scriptTab, message=FALSE))
    
   # validate
   validate(need(input$scriptID2, message="Need a script ID"), 
            #need(!input$scriptID2 %in% scriptTab$ID, message="Duplicated ID name"), 
            
            need(input$domain, message="Need specify domain"), 
            need(input$category, message="Need specify category"), 
            need(input$keys, message="Need specify keys")
            
            #need(input$userName, message="Need specify userName")
   )
            
   DOMAIN= input$domain
   CATEGORY = input$category
   KEY = input$keys
   ID = input$scriptID2
   USER.NAME =  globalVars$login$userName  # "feng.yang@regeneron.com" # input$userName
   VALUE = input$scriptArea
   DATE = as.Date(as.POSIXlt(Sys.time(), tz="America/New_York"), format = "yyyy-mm-dd") %>% as.character()
    
   scriptTab = scriptTab %>% mutate(DATE=as.character(DATE))
   scriptTab = rbind(scriptTab, c(DOMAIN,	USER.NAME,	CATEGORY,	ID,	KEY,	DATE,	VALUE))
 
   globalVars$scriptTab= scriptTab
    
   
 })
 
 
 
 
 
 
 
  output$script_container <- renderUI({
    scriptTab = filterData()
    validate(need(scriptTab, message=FALSE))
   
    i = values[["iScript"]]
    value =  scriptTab$VALUE[i]
    textAreaInput(ns("scriptArea"), label=NULL, value= value,  rows=25, placeholder = "add your script here.")
     
      # output$htmlscript <- renderUI({
      #    i = values[["iScript"]]
      #    value =  scriptTab$VALUE[i]
      #   # textAreaInput(ns("scriptArea"), label=NULL, value= NULL,  rows=25, placeholder = "data %>% mutate(TEST=1)")
      #   value = gsub("\n", '<br/>', value, fixed=TRUE)
      #   
      #   HTML(value)
      # 
      # })
      #htmlOutput(ns("htmlscript"))
      # verbatimTextOutput(ns("textAreascript"))
      #output$textAreascript <- renderUI({
       
      
      #if(input$search_or_add=="search library") {
      #  htmlOutput(ns("htmlscript"))
      #}else{  # "add script"
        #scriptTab = filterData()
        

    
  })

  
  
}


 


