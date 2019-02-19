################################################################################
# module_build_dataset_UI
################################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_build_dataset_v2_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to  
                          1) align the variable (column) against the standard one [column], <br> 
                          2) standardize the input dataset using script [script], <br>
                          3) manual curation and confirmation [curation], <br>
                          3) render the resulting data.", color="gray")))
      ),
      
  tabBox(width=12, id = ns("run_script_for_dataset_construction"), title =NULL, 
       
       # checkInCols_container 
       tabPanel(width=12, title="checkInCols", value = "checkIn_columns", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("checkInCols_container")))) 
       ),        
       
       # script_container 
       tabPanel(width=12, title="runScript", value = "run_script", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("script_container"))))  
       ),     
           
       # checkInRows_container
       tabPanel(width=12, title="checkInRows", value = "checkIn_rows", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("checkInRows_container"))))
       ), 
       
       # data_container
       tabPanel(width=12, title="output", value = "output", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("data_container"))))
       )
  ) 
  )
}

################################################################################
# module_build_dataset
################################################################################

module_build_dataset_v2 <- function(input, output, session, 
                              ALL, dataset_name, script, default_checkin
                              )  {
  
  ns <- session$ns
  values <- reactiveValues(data=NULL,figure=NULL,table = NULL)
  values2 <- reactiveValues(data=NULL,figure=NULL,table = NULL)
  
  ################################
  # UI for dataset_container
  ################################
  output$checkInCols_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))
     
    # call module
    ALL = callModule(module_checkInCols, "checkInCols",  
               ALL, 
               dataset_name=dataset_name, 
               default_checkin = default_checkin 
    )
     
    # UI
    fluidRow(column(12, 
                    module_checkInCols_UI(ns("checkInCols"), label = NULL)
             )
    )
  })  
   
     
  ################################
  # UI for script_container
  ################################
  output$script_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
     
    tagList(
      fluidRow(column(12, 
                      HTML(colFmt("Note, by running/modifying the following script template, 
                                  you can perform any manipulation upon the input data (dataset). 
                                  Certain key words should not be changed; certain formatting should be followed. 
                                  See instruction carefully.", color="gray"))
                      )
               ), 
      
      fluidRow(
        column(3, 
               actionButton(ns("run_script"), label="Run script", style=actionButton.style )
        ), 
        column(9, uiOutput(ns("run_script_message"))
        )
      ),
      
      fluidRow(
        column(12,
               textAreaInput(ns("script_content"), label=NULL, value=script, 
                             rows=100,  #cols=200,
                             width = '750px',   #400px', or '100%'
                             placeholder= "Your script here."
                             ) 
        )
      )
    )
  })
   
  output$run_script_message <- renderUI(renderText({
    { values$run_script_message } 
  })() %>% HTML())
  
  

  ################################
  # data_container
  ################################
  output$data_container <- renderUI({  
 
    print("in data_container")
       # why this not working as expected    
       #   ALL = callModule(module_save_data, paste0("module_save_data_", 1), 
       #                    ALL,
       #                    data = ALL$DATA[[dataset_name]] ,   
       #                    data_name =  dataset_name    #names(values$data[i])
       #   )
       # module_save_data_UI(ns(paste0("module_save_data_", 1)), label = NULL) 
       #   
  
    tdata =  inputData() #ALL$DATA[[dataset_name]]   
    validate(need(tdata, message="no data found"))                                                                                                                                      
     
    fluidRow( 
       fluidRow( 
         column(width=3, 
                uiOutput(ns("data_name_container"))),                                                                                                                             
         
         column(width=2, 
                actionButton(ns("saveit"),label="Save it", style=actionButton.style)),                                                                                            
         
         column(width=2,                                                                                                                                                                   
                downloadButton(ns("downloadcsv"),label="csv", icon=icon("download"), style=actionButton.style)                                                                        
                ), 
         
         column(width=3, 
                h6("Select page length:"), align="right"),  #offset = 2,),    
                
         column(width=2, align="left",  #offset = 2,                                                                                                                                       
                selectInput(ns("pageLength"), label = NULL,                                                                                                        
                            width="100%",                                                                                                                                                  
                            choices = seq(1, 100, by=1),                                                                                                                                   
                            selected = 3)                                                                                                                                                  
         )
      ), 
         
      fluidRow(column(width=12, DT::dataTableOutput(ns("mydatatable")))),
       fluidRow(
           column(width=12,                                                                                                                                                                   
                  uiOutput(ns("column_selector"))                                                                                                                                            
           )     
         )
       )
  })
   
  inputData <- reactive({
    ALL$DATA[[dataset_name]]
  })
  
  output$data_name_container <- renderUI({                                                                                                                                             
    validate(need(globalVars$login$status, message=FALSE))                                                                                                                             
    
    textInput(ns("data_name"), value=NULL, placeholder="data name", width="100%", label=NULL)                                                                                                       
  })
  
  output$mydatatable <- DT::renderDataTable(                                                                                                                                          
    DT::datatable(data = inputData(),                                                                                                                                                       
                  options = list(pageLength = input$pageLength, 
                                 lengthChange = FALSE, width="100%", scrollX = TRUE)                                                                   
    ))
  
  # selected_data                                                                                                                                                                       
  output$column_selector <- renderUI({  
    data = inputData()
    validate(need(data, message=FALSE))                                                                                                                                                
    
    col.lst = c("", colnames(data))                                                                                                                                                    
    selectizeInput(ns("column_name_lst"),                                                                                                                                               
                   width="100%",                                                                                                                                                        
                   label    = "select columns",                                                                                                                                 
                   choices  = colnames(data),                                                                                                    
                   multiple = TRUE,                                                                                                                                                     
                   selected = colnames(data))                                                                                                 
  })                                                                                                                                                                                    
  
  
  filtered_data <- reactive({  
    data = inputData()
    validate(need(data, message="no data found"),                                                                                                                                      
             need(input$column_name_lst, message="no column selected")                                                                                                                  
    )      
    data %>% select(one_of(input$column_name_lst))                                                                                                                                     
  })
                                                                                                                        
  # save data                                                                                                                                                                            
  observeEvent(input$saveit, {                                                                                                                                                          
    
    validate(need(input$saveit, message=FALSE), 
             need(ALL$DATA, messag="No data found"),
             need(input$data_name, message="please specify data name")
    )  
    ALL$DATA[[input$data_name]] <- filtered_data()                                                                                                                                                  
    
  })
  
  #downloadcsv
  output$downloadcsv <- downloadHandler(                                                                                                                                                
    filename = function() {                                                                                                                                                             
      # data_name = attr(data, "name")                                                                                                                                                 
      ifelse(is.null(input$data_name),                                                                                                                                           
              paste0("download.csv", sep = ""),                                                                                                                                        
              paste0(input$data_name, ".csv", sep = ""))                                                                                                                                     
                                                                                                                                                                     
    },                                                                                                                                                                                  
    content = function(file) {                                                                                                                                                          
      write_csv(filtered_data(), path=file )                                                                                                                                            
    }                                                                                                                                                                                   
  )                                                                                                                                                                 
  
  
  
  ################################
  # checkInRows_container
  ################################
  output$checkInRows_container <- renderUI({  
    validate(need(is.list(values$table), message="No table found, or values$table needs to be a list"))
    
    print("in checkInRows_container")
     
    tagList(
      fluidRow(  
        column(2, offset=10,
               actionButton(ns("saveAll"),label="Save all", style=actionButton.style)
        ) 
      ),
  
      # all curation table   
      lapply(1:length(names(values$table)), function(i) {
        
        validate(need(values$table[[i]], message="no table found"), 
                 #need(nrow(values$table[[i]])>0, message="no table found"),   # can't have his line
                 need(is.data.frame(values$table[[i]]), message="only data.frame allowed"),
                 need(values$data, message="no data found")
        )
        
        # based on 'data', manually curate it based on table, and finally save to ALL  
        # ALL = callModule(module_checkInRows, paste0("module_save_table_", i), 
        #                ALL, dataset_name,
        #                data = values$data,
        #                table = values$table[[i]], 
        #                table_index = i, 
        #                table_name = names(values$table[i])
        #                )
        
        # values$table[[i]] = 
        #isolate({ 
          values = callModule(module_checkInRows_v2, paste0("module_save_table_", i), 
                         values0 = values,
                         table_index = i
        )      
        #})
        
        # UI
        module_checkInRows_v2_UI(ns(paste0("module_save_table_", i)), label = NULL) 
      })
    
    ) # tagList
  })
   
  
  
  
  observeEvent(input$saveAll, {
    validate(need(input$saveAll, message=NULL), 
             need(values$table, message=NULL), 
             need(values$data, message=NULL)
    ) 
     
    tdata = values$data
    
    ntabl = length(values$table)
    
    for (i in 1:ntabl) {
      table = values$table[[i]]
      if (is.null(table) ) {next} 
      if (nrow(table)==0) {next} 
      
      print("inside for loop")
      print(i)
      print(nrow(table))
      print(table)
      
      # incompatible types (character / logical)
      KEY = attr(table, "key")
      tdata[, KEY] = as.character(tdata[, KEY])
      table[, KEY]  = as.character(table[, KEY])
      
      col.lst =  setdiff(colnames(table), KEY)
      tdata = tdata %>% select(-one_of(intersect(col.lst, colnames(tdata)))) %>% 
        left_join(table, by=KEY)
    }
    
    ALL$DATA[[dataset_name]] <- tdata 
    print("checkInRows sucessful")
    
  })
  
  ################################
  # run_script  
  ################################ 
  observeEvent(input$run_script, {
    validate(need(input$script_content, message="no script loaded yet")
    ) 
      
    ihandbook = 1
    dataset = ALL$DATA[[dataset_name]]
 
    output= NULL
    # source the function
    message= tryCatch(eval(parse(text=(input$script_content))), 
              error=function(e) {
                print("error found in runing script"); 
                return(NULL)
              } #, finally = {
              # eval(parse(text=txt)) %>% as.data.frame()
              #}
     )  
    if (!is.null(output$data)) {eval(parse(text="values$data = output$data"))}
    if (!is.null(output$table)) {eval(parse(text="values$table = output$table"))}
  
     
    if (is.null(message) ) {
      values$run_script_message = colFmt("error: run script failed,  no data/table generated",'red')
     }else{
      values$run_script_message = colFmt("run script sucessfully",'green')
    }
      
  })
  
  return(ALL)
}



