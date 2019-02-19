 
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_run_script_UI <- function(id, label = "") {
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
      
  tabBox(width=12, id = ns("run_script_for_data_figure_table"), title =NULL, 
       
       # dataset_container 
       tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("dataset_container")))) 
                
       ),       
       
       
       # script_container 
       tabPanel(width=12, title="script", value = "script", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("script_container"))))  
       ),     
           
       # data_container
       tabPanel(width=12, title="data", value = "data", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("data_container"))))
       ),
       
       # table_container
       tabPanel(width=12, title="table", value = "table", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("table_container"))))
       ),
       
       #figure_container
       tabPanel(width=12, title="figure", value = "figure", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("figure_container"))))   
       )
  ) 
  )
}



################################################################################ 
################################################################################
# module_run_script
################################################################################
################################################################################
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn't part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

module_run_script <- function(input, output, session, 
                              ALL, dataset, script
                              )  {
  
  ns <- session$ns
   
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
   
  #globalVars = NULL
  #globalVars$login$status = TRUE
    
  values <- reactiveValues(data=NULL, figure=NULL,table = NULL)
  
  print("in module_run_script:::")
  
  ################################
  # UI for dataset_container
  ################################
  output$dataset_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))
    
    callModule(module_table_output, "loaded_dataset", mytab=dataset)
   
    fluidRow(column(12, 
                    module_table_output_UI(ns("loaded_dataset"), label = "loaded_dataset")
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
                                  you will be able to generate any derived dataset(s), table(s) and figure(s) 
                                  based on the input data (dataset) with the parameters (param). 
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
  # figure_container
  ################################
  output$figure_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    tagList(
    fluidRow(
      column(12,  offset=10,
           actionButton(ns("save_all_figure"),label="Save all", style=actionButton.style)
            )
      ), 
    
    lapply(1:length((values$figure)), function(i) {
      validate(need(values$figure[[i]], message="no figure found"), 
               need(is.ggplot(values$figure[[i]]), message="only ggpot object allowed")
      )
      
      # save values$figure into ALL$FIGURE
      isolate({ 
          ALL = callModule(module_save_figure, paste0("module_save_figure_", i), 
                           ALL, 
                           figure = values$figure[[i]], 
                           figure_index = i, 
                           figure_name=names(values$figure[i]), 
                           figure_data = values$figure[[i]]$data
          )
      })
      module_save_figure_UI(ns(paste0("module_save_figure_", i)), label = NULL) 
    })
    
    ) # tagList
  })
  
  
  ################################
  # data_container
  ################################
  output$data_container <- renderUI({  
    validate(need(is.list(values$data), message="No data found, or values$data needs to be a list"))
    
    tagList(
      fluidRow(
        column(12, offset=10, 
               actionButton(ns("save_all_data"),label="Save all", style=actionButton.style)
        )
      ),
      
    lapply(1:length(names(values$data)), function(i) {
      validate(need(values$data[[i]], message="no data found"), 
               need(is.data.frame(values$data[[i]]), message="only data.frame allowed")
      )
     
      isolate({ 
        ALL = callModule(module_save_data, paste0("module_save_data_", i), 
                         ALL,
                         data = values$data[[i]],   
                         data_name =  names(values$data[i])
        )
      })
      module_save_data_UI(ns(paste0("module_save_data_", i)), label = NULL) 
    })
    
    )
  })
  
  
  
  ################################
  # tabl_container
  ################################
  output$table_container <- renderUI({  
    validate(need(is.list(values$table), message="No table found, or values$table needs to be a list"))
    
    tagList(
      fluidRow(
        column(12, offset=10,
               actionButton(ns("save_all_table"),label="Save all", style=actionButton.style)
        )
      ),
      
    lapply(1:length(names(values$table)), function(i) {
      validate(need(values$table[[i]], message="no table found"), 
               need(is.data.frame(values$table[[i]]), message="only data.frame allowed")
      )
      
      # save values$table into ALL$TABLE
      isolate({ 
        ALL = callModule(module_save_table, paste0("module_save_table_", i), 
                        ALL,
                        table = values$table[[i]], 
                        table_index = i, 
                        table_name = names(values$table[i])
                        )
      })
      module_save_table_UI(ns(paste0("module_save_table_", i)), label = NULL) 
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
    #isolate({ 
      
    # eval(parse(text=(input$script_content)))   removed 02/10/2019
    output= NULL
    # source the function
     tryCatch(eval(parse(text=(input$script_content)))  , 
              error=function(e) {
                print("error found in runing script"); 
                return(NULL)
              } #, finally = {
              # eval(parse(text=txt)) %>% as.data.frame()
              #}
     )  
    # if(is.function(output)) { print("output is a function"); output = NULL}
    # 
    # if(is.null(output)) {values$run_script_message = colFmt("error found in runing script",'red')}
    # validate(need(output, message="error found in runing script"))
    # 
    print("run script sucessfully") 
  
    data.generating<- tryCatch(eval(parse(text="values$data = output$data")), 
                                 error=function(e) {
                                   print("warning: run script sucessfully, but no data generating by runing script"); 
                                   return(NULL)
                                 } , finally = {
                                   ("sucess!")
                                 }
    )
    
    figure.generating<- tryCatch(eval(parse(text="values$figure = output$figure")), 
                      error=function(e) {
                        print("warning: run script sucessfully, but no figure generating by runing script"); 
                        return(NULL)
                      } , finally = {
                        ("sucess!")
                      }
    )
    
    table.generating<- tryCatch(eval(parse(text="values$table = output$table")), 
                              error=function(e) {
                                print("warning: run script sucessfully, but no table generating by runing script"); 
                                return(NULL)
                              } , finally = {
                                 ("sucess!")
                              }
    ) 
     
    if (is.null(figure.generating) & is.null(table.generating) & is.null(data.generating) ) {
      values$run_script_message = colFmt("error: run script sucessfully, but no data/figure/table generating by runing script",'red')
      #updateTextInput(session, "run_script_message", value="error: run script sucessfully, but no figure and table generating by runing script")
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
  
  
  observeEvent({input$save_all_figure}, {
    validate(need(length(values$figure), message="no figure found") )
    isolate({ 
    #lapply(1:length(values$figure), function(i) ALL$FIGURE[[names(values$figure)[i]]] = values$figure[[i]])
      ALL$FIGURE = (c(ALL$FIGURE, values$figure))
      
    print("all figure have been saved")
    print(names(ALL$FIGURE))
    })
    
  })
  
  
  
 # }) # isolate 
  return(ALL)
}



