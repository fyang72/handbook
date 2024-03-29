#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------
module_run_script_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to 
                         1) render the source dataset if provided (dataset tab), 
                         2) apply scripts upon the source dataset (script tab), 
                         3) render the derived data (data tab), final table (table tab) and figure (figure tab).", 
                         color="gray")))
      ),
      
    tabBox(width=12, id = ns("run_script_for_data_figure_table"), title =NULL, 
       
       # dataset_container 
       tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  tagList( 
                      
                    fluidRow(
                      HTML(colFmt("The following commonly used filters can be used to narrow down your dataset<br>", color="gray")
                      ),
                      uiOutput(ns("select_TEST_container")), 
                      uiOutput(ns("select_ARMA_container")),
                      style='margin-bottom:30px;  border:1px solid; padding: 10px;'
                     ),
                    fluidRow(
                      column(12, 
                             uiOutput(ns("dataset_container")), 
                             style='margin-bottom:30px;  border:1px solid; padding: 10px;'
                             )
                      ) 
                  )
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
  ) # tagList
}

################################################################################ 
################################################################################
# module_run_script
################################################################################
################################################################################ 

module_run_script <- function(input, output, session, 
                              ALL, dataset, script, params=NULL
                              )  {

  # script is a list of file names containing the "script"
  script <- sapply(script, function(i) paste0(readLines(i), collapse="\n")) %>% unlist()
  
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL, table = NULL)

################################
# UI for dataset_container
################################
  output$select_STUDYID_container <- renderUI({ 
    tdata = dataset
    validate(need(tdata, message="no dataset found yet"),  
             need("STUDYID" %in% colnames(tdata), message=FALSE) 
    )
    
    studyid_lst = c(unique(tdata%>%drop_na(STUDYID)%>%pull(STUDYID))) 
    validate(need(length(studyid_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    #  column(6,
    inline(
      selectizeInput(ns("which_studyid"), 
                     label    = "select which study", 
                     choices  = studyid_lst, 
                     multiple = TRUE,
                     width="100%", 
                     selected = studyid_lst[1]
      )
    )
    #  )
    # )
    
  })
  
  
  output$select_TEST_container <- renderUI({ 
    tdata = dataset
    validate(need(tdata, message="no dataset found yet"),  
             need("TEST" %in% colnames(tdata), message=FALSE) 
    )
    
    test_lst = c(unique(tdata%>%drop_na(TEST)%>%pull(TEST))) 
    validate(need(length(test_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    #  column(6,
    inline(
        selectizeInput(ns("which_test"), 
                  label    = "select which analyte", 
                  choices  = test_lst, 
                  multiple = FALSE,
                  width="100%", 
                  selected = test_lst[1]
                  )
    )
    #  )
   # )
        
  })
  
  
output$select_ARMA_container <- renderUI({ 
  tdata = dataset
  validate(need(tdata, message="no dataset found yet"), 
           need("ARMA" %in% colnames(tdata), message=FALSE) 
  )
   
  arma_lst = c(unique(tdata%>%drop_na(ARMA)%>%pull(ARMA)))
  validate(need(length(arma_lst)>1, message=FALSE)) 
  
  inline = function (x) {
    tags$div(style="display:inline-block;", x)
  }
  
  #fluidRow(
  #  column(6, 
  inline(
      selectizeInput(ns("which_arma"), 
                    label    = "select which dose group(s)", 
                    choices  = arma_lst, 
                    multiple = TRUE,
                    width="100%", 
                    selected = arma_lst
                    )
  )
   # )
  #)
})
  
  
output$dataset_container <- renderUI({  
 
  tdata = filtered_dataset()
  validate(need(tdata, message="no data found yet")  
  )
  
  callModule(module_save_data, "loaded_dataset", ALL, 
             data=tdata, 
             data_name="")
  
  fluidRow(
    column(12, module_save_data_UI(ns("loaded_dataset"), label = "loaded_dataset"))
  )
}) 


################################
# UI for script_content_container
################################
output$script_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
 
  tagList(
    fluidRow(column(12,
                    HTML(colFmt("Step: 1) select which script template, 2) click 'run script', 3) modify
                                the script if needed, then re-run it.", color="gray"))
                    )
             ),

    fluidRow(
      column(9,
              selectizeInput(ns("script_selector"),
                               label    = NULL, #"select script:" ,
                               choices  = names(script),
                               multiple = FALSE,
                               selected = names(script)[1]
                             )
      ),
      column(3,
             actionButton(ns("run_script"), label="Run script", style=actionButton_style )
      )
    ),
    
    fluidRow(column(12, uiOutput(ns("script_content_container"))))
   
  )
})
  

output$script_content_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
  
  aceEditor(ns("script_content"), 
            mode="r", 
            value=if (is.null(names(script))) {script} else {script[input$script_selector]}, 
            theme = "crimson_editor",   # chrome
            autoComplete = "enabled",
            height = "1000px", 
            fontSize = 15 
  )
  
})
  
################################
# data_container
################################
output$data_container <- renderUI({  
  validate(need(is.list(values$data), message="No data found, or values$data needs to be a list"))
  
  tagList(
    fluidRow(
      column(12, offset=10, 
             actionButton(ns("save_all_data"),label="Save all", style=actionButton_style)
      )
    ),
    
    lapply(1:length(names(values$data)), function(i) {
      validate(need(values$data[[i]], message="no data found"), 
               need(is.data.frame(values$data[[i]]), message="only data.frame allowed")
      )
      
      ALL = callModule(module_save_data, paste0("module_save_data_", i), 
                       ALL,
                       data = values$data[[i]],   
                       data_name =  names(values$data[i])
      )
      
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
             actionButton(ns("save_all_table"),label="Save all", style=actionButton_style)
      )
    ),
    
    lapply(1:length(names(values$table)), function(i) {
      validate(need(values$table[[i]], message="no table found"), 
               need(is.data.frame(values$table[[i]]), message="only data.frame allowed")
      )
      
      # save values$table into ALL$TABLE
      ALL = callModule(module_save_table, paste0("module_save_table_", i), 
                      ALL,
                      table = values$table[[i]], 
                      table_index = i, 
                      table_name = names(values$table[i])
                      )
      
      module_save_table_UI(ns(paste0("module_save_table_", i)), label = NULL) 
    })
  
  )
})
  

################################
# figure_container
################################
output$figure_container <- renderUI({  
  validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
  
  
  
  tagList(
    fluidRow(
      column(12,  offset=10,
             actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
      )
    ), 
    
    lapply(1:length((values$figure)), function(i) {
      validate(need(values$figure[[i]], message="no figure found"), 
               need(is.ggplot(values$figure[[i]]), message="only ggpot object allowed")
      )
      
      # save values$figure into ALL$FIGURE
      ALL = callModule(module_save_figure, paste0("module_save_figure_", i), 
                       ALL, 
                       figure = values$figure[[i]], 
                       figure_index = i, 
                       figure_name = names(values$figure[i]), 
                       figure_data = values$figure[[i]]$data
      )
      
      module_save_figure_UI(ns(paste0("module_save_figure_", i)), label = NULL) 
    })
    
  ) # tagList
})


filtered_dataset <- reactive({

  validate(need(dataset, message="no dataset"))
  
  if (class(dataset) == "xpose.data") {
    tdata = dataset %>% slot("Data")
  }else if (class(dataset) == "list"  & all(c("adsl", "adex", "adpc") %in% names(dataset))) {
    tdata = dataset[["adsl"]]
  }else {
    tdata = dataset
  }
  
  if ("STUDYID" %in% colnames(tdata) && !is.null(input$which_studyid)) {
    tdata = tdata %>% filter(STUDYID %in% input$which_arma)
  } 
  
  if ("TEST" %in% colnames(tdata) && !is.null(input$which_test)) {
    tdata = tdata %>% filter(TEST %in% input$which_test)
  }
  
  if ("ARMA" %in% colnames(tdata) && !is.null(input$which_arma)) {
    tdata = tdata %>% filter(ARMA %in% input$which_arma)
  } 
   
  tdata
})




################################
# run_script  
################################ 
observeEvent(input$run_script, {
  validate(need(input$script_content, message="no script loaded yet")
  )

  ihandbook = 1
  
  environment(try_eval) <- environment()
  env = try_eval(text=input$script_content)
  
  output=NULL; error_message=NULL
  if ("output" %in% ls(env)) {output = get("output", env)}
  if ("message" %in% ls(env)) {error_message = get("message", env)}
  
  #removeNotification("myid")
  if ((length(error_message)==0 & !is.null(output))) {
    if("data" %in% names(output)) {values$data = output$data}
    if("figure" %in% names(output))   {values$figure = output$figure}
    if("table" %in% names(output)) {values$table = output$table}
    showNotification("run script sucessfully", type="message", id="myid") 
  }else{
    showNotification(paste0(error_message, collapse="\n"), type="error", id="myid")
  }
   
 })

   
  
#--------------------------------------  
# observeEvent  
#-------------------------------------- 
# https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw

observeEvent({input$save_all_table}, {
  validate(need(length(values$table), message="no table found") )
  
  #lapply(1:length(values$table), function(i) ALL$TABLE[[names(values$table)[i]]] = values$table[[i]])
  ALL$TABLE = (c(ALL$TABLE, values$table))
  showNotification("all tables saved", type="message") 
 
})


observeEvent({input$save_all_data}, {
  validate(need(length(values$data), message="no data found") ) 
 
  #lapply(1:length(values$data), function(i) ALL$DATA[[names(values$data)[i]]] = values$data[[i]])
  ALL$DATA = (c(ALL$DATA, values$data))
  showNotification("all data saved", type="message") 
 
})


observeEvent({input$save_all_figure}, {
  validate(need(length(values$figure), message="no figure found") )
  isolate({ 
  #lapply(1:length(values$figure), function(i) ALL$FIGURE[[names(values$figure)[i]]] = values$figure[[i]])
    ALL$FIGURE = (c(ALL$FIGURE, values$figure))
     
  showNotification("all figures saved", type="message") 
  })
    
})
  
return(ALL)
}



