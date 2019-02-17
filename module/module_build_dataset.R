 
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_build_dataset_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to 1) render the source dataset (dataset tab), 
                         2) perform the work based on the source dataset (script tab), 
                         3) render the secondary data (data tab), final table (table tab) and figure (figure tab).", color="gray")))
      ),
      
  tabBox(width=12, id = ns("run_script_for_dataset_construction"), title =NULL, 
       
       # checkInCols_container 
       tabPanel(width=12, title="column", value = "align_column", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("checkInCols_container")))) 
                
       ),        
       
       # script_container 
       tabPanel(width=12, title="script", value = "script", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("script_container"))))  
       ),     
           
       # table_container
       tabPanel(width=12, title="curation", value = "curation", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("table_container"))))
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
################################################################################
# module_build_dataset
################################################################################
################################################################################
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn't part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

module_build_dataset <- function(input, output, session, 
                              ALL, dataset_name, script, default_checkin
                              )  {
  
  ns <- session$ns
   
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
   
  #globalVars = NULL
  #globalVars$login$status = TRUE
    
  values <- reactiveValues(data=NULL,figure=NULL,table = NULL)
   
  
  print("in module_build_dataset")
  
  ################################
  # UI for dataset_container
  ################################
  output$checkInCols_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))
    
    print("in checkInCols_container")
    
    #isolate({ 
      ALL = callModule(module_checkInCols, "adpx_checkInCols",  
                 ALL, 
                 dataset_name=dataset_name, #"mYtEsT_for_construct_dataset", 
                 default_checkin = default_checkin 
      )
      
    #})
    #values$data[["data_name"]] = ALL$DATA[[dataset_name]] 
    
    fluidRow(column(12, 
                    module_checkInCols_UI(ns("adpx_checkInCols"), label = "checkInCols")
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
                                  you can perform any manipulation upon the input data (dataset) with the parameters (param). 
                                  Certain key words should not be changed; certain formatting should be followed. 
                                  See instruction carefully.", color="gray"))
                      )
               ), 
      
      fluidRow(
        column(3, 
               actionButton(ns("run_script"), label="Run script", style=actionButton.style )
        ), 
        column(9, uiOutput(ns("run_script_message"))
                 #HTML(colFmt("values4nmdat$colnames.in.stdlist$info",'red'))
                 # textInput(ns("run_script_message"), 
                 #           width="100%",
                 #           label= NULL, 
                 #           value=  NULL,
                 #           placeholder = "sucess/fail of running script will be shown here."
                 #           )
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
    #validate(need(is.list(values$data), message="No data found, or values$data needs to be a list"))
   
    print("in data_container")
    
        ALL = callModule(module_save_data, paste0("module_save_data_", 1), 
                         ALL,
                         data = ALL$DATA[[dataset_name]],  #values$data[[i]],   
                         data_name =  dataset_name    #names(values$data[i])
        )
       
      module_save_data_UI(ns(paste0("module_save_data_", 1)), label = NULL) 
   # })
    
    #)
  })
  
  
  
  ################################
  # tabl_container
  ################################
  output$table_container <- renderUI({  
    validate(need(is.list(values$table), message="No table found, or values$table needs to be a list"))
    
    print("in table_container")
    
    tagList(
      fluidRow(
        column(12, offset=10,
               actionButton(ns("save_all_table"),label="Save all", style=actionButton.style)
        )
      ),
      
      #
      
    lapply(1:length(names(values$table)), function(i) {
      
      validate(need(values$table[[i]], message="no table found"), 
               #need(nrow(values$table[[i]])>0, message="no table found"), 
               need(is.data.frame(values$table[[i]]), message="only data.frame allowed"),
               need(values$data, message="no data found")
      )
      
      # save values$table into ALL$TABLE
      #isolate({ 
         ALL = callModule(module_checkInRows, paste0("module_save_table_", i), 
                         ALL, dataset_name,
                         data = values$data,
                         table = values$table[[i]], 
                         table_index = i, 
                         table_name = names(values$table[i])
                         )
         
       #  if (!is.null(tmp)) { values2$data =tmp}
      #})  
         
        module_checkInRows_UI(ns(paste0("module_save_table_", i)), label = NULL) 
      
      #})
      
      
    })
    
    )
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
  
    
    
  #--------------------------------------  
  # observeEvent  
  #-------------------------------------- 
  # https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw
  
  observeEvent({input$save_all_table}, {
    validate(need(length(values$table), message="no table found") )
    isolate({ 
      #lapply(1:length(values$table), function(i) ALL$TABLE[[names(values$table)[i]]] = values$table[[i]])
    
      ALL$TABLE = (c(ALL$TABLE, values$table))
    print("all table have been saved")
    print(names(ALL$TABLE))
    })
    
  })
  
  
  observeEvent({input$save_all_data}, {
    validate(need(length(values$data), message="no data found") )
    #ALL$DATA[[data_name]]  = load_external_data()
    isolate({ 
    #lapply(1:length(values$data), function(i) ALL$DATA[[names(values$data)[i]]] = values$data[[i]])
      ALL$DATA = (c(ALL$DATA, values$data))
      
    print("all data have been saved")
    print(names(ALL$DATA))
    })
  })
  
  
  return(ALL)
}



