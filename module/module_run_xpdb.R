#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------
module_run_xpdb_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to 
                         1) load the source dataset (dataset tab), 
                         2) apply scripts upon the source dataset (script tab), 
                         3) render the derived data (data tab), final table (table tab) and figure (figure tab).", 
                         color="gray")))
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
           
           #basic_gof_figure_container 
           tabPanel(width=12, title="basic-GOF", value = "basic_gof", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("basic_gof_container"))))   
           ), 
           
           tabPanel(width=12, title="indiv-plots", value = "indiv_plots", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("indiv_plots_container"))))   
           ), 
           
           tabPanel(width=12, title="distribution-plots", value = "distribution_plots", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("distribution_plots_container"))))   
           ),  
           
           tabPanel(width=12, title="vpc-plots", value = "vpc_plots", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("vpc_plots_container"))))   
           ),  
           
           tabPanel(width=12, title="other-plots", value = "other_plots", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("other_plots_container"))))   
           )
    ) 
    ) # tagList
}

################################################################################ 
################################################################################
# module_run_script
################################################################################
################################################################################ 

module_run_xpdb <- function(input, output, session, 
                              ALL, dataset, dataset_name="", script="", params=NULL
)  {
  
  # script is a list of file names containing the "script"
  script <- sapply(script, function(i) paste0(readLines(i), collapse="\n")) %>% unlist()
  
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL, table = NULL)
  
  user.name = tolower(Sys.info()["user"])  # determineCurrentUser()
    
  
  ################################
  # UI for dataset_container
  ################################ 
  output$dataset_container <-renderUI({
    
    fluidRow( 
      
      # dataset
      #HTML(colFmt("Step 1: Load the xpdb dataset<br>", color="darkblue")
      #),
     
      fluidRow(
        column(width=4, uiOutput(ns("select_which_program_container"))), 
        column(width=4, uiOutput(ns("select_which_runno_container")))  
      ),
   
     # fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
       
      #HTML(colFmt("Step 2: Review the dataset <br>", color="darkblue")
     # ),
      uiOutput(ns("render_dataset_container")), 
      style='margin-bottom:30px;  border:1px solid; padding: 10px;'
      
    ) 
    
  }) 
  

  
output$select_which_program_container <- renderUI({
  dir_lst = list.files(path = paste0(HOME, "/output/", user.name, "/"), 
                       full.names = FALSE, recursive = FALSE)
  dir_lst = c("", dir_lst)
  selectizeInput(ns("which_program"), 
                 label    = "Select program:", 
                 choices  = dir_lst, 
                 multiple = FALSE,
                 selected = dir_lst[1]) 
  
})

output$select_which_runno_container <- renderUI({
  dir_lst = list.files(path = paste0(HOME, "/output/", 
                                     user.name, "/", 
                                     input$which_program, "/ctl/"), 
                       full.names = FALSE, recursive = FALSE)
  dir_lst = c("", dir_lst)
  selectizeInput(ns("which_runno"), 
                 label    = "Select run:", 
                 choices  = dir_lst, 
                 multiple = FALSE,
                 selected = dir_lst[1]) 
  
})

 
################################
# Load dataset
################################ 
xpdb_inputData <- reactive({
  
  # read xpdb using xpose.data
 
    validate(need(input$which_program, message=FALSE), 
             need(input$which_runno, message=FALSE) 
    )  
    
    #install.packages('xpose')
    #devtools::install_github('UUPharmacometrics/xpose')
    # https://uupharmacometrics.github.io/xpose/index.html
    #@ 
  library(xpose)
  
   file.dir = paste0(HOME, "/output/", 
              "feng.yang", "/", 
              "TEST", "/ctl/", 
              "LN102_DAT1", "/")
   
   file.dir = paste0(HOME, "/output/", 
                     "feng.yang", "/", 
                     input$which_program, "/ctl/", 
                     input$which_runno, "/")
   
   xpdb <- xpose_data(runno='001', prefix = "run", file = 'run001.lst', dir = file.dir)
  
   #summary(xpdb)
   
   xpdb_ex_pk
  
})  
  
  
  output$render_dataset_container <- renderUI({  
     
    tdata = xpose::get_data(xpdb_inputData())
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
  # basic_gof_container
  ################################
  output$basic_gof_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    # basic GOF plots
    fig_name_lst <- intersect(names(values$figure), basic_gof_plots_lst)
    fig_lst <- values$figure[fig_name_lst]
    validate(need(length(fig_lst)>0, message="no figure found"))
    
    tagList(
      fluidRow(
        column(12,  offset=10,
               actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
        )
      ), 
      
      lapply(1:length((fig_lst)), function(i) {
        validate(need(fig_lst[[i]], message="no figure found"), 
                 need(is.ggplot(fig_lst[[i]]) | class(fig_lst[[i]])[1]=="plotly", 
                      message="only ggplot/plotly object allowed")
        )
        
        # save fig_lst into ALL$FIGURE
        ALL = callModule(module_save_figure, paste0("module_save_figure_basic_gof", i), 
                         ALL, 
                         figure = fig_lst[[i]], 
                         figure_index = i, 
                         figure_name = names(fig_lst[i]), 
                         figure_data = NULL
        )
        
        module_save_figure_UI(ns(paste0("module_save_figure_basic_gof", i)), label = NULL) 
      })
      
    ) # tagList
  })
  
  
  
  ################################
  # indiv_plots_container
  ################################
  output$indiv_plots_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    # basic GOF plots
    fig_name_lst <- intersect(names(values$figure), indiv_plots_lst)
    fig_lst <- values$figure[fig_name_lst]
    validate(need(length(fig_lst)>0, message="no figure found"))
    
    tagList(
      fluidRow(
        column(12,  offset=10,
               actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
        )
      ), 
      
      lapply(1:length((fig_lst)), function(i) {
        validate(need(fig_lst[[i]], message="no figure found"), 
                 need(is.ggplot(fig_lst[[i]]) | class(fig_lst[[i]])[1]=="plotly", 
                      message="only ggplot/plotly object allowed")
        )
        
        # save fig_lst into ALL$FIGURE
        ALL = callModule(module_save_figure, paste0("module_save_figure_indiv_plots", i), 
                         ALL, 
                         figure = fig_lst[[i]], 
                         figure_index = i, 
                         figure_name = names(fig_lst[i]), 
                         figure_data = NULL
        )
        
        module_save_figure_UI(ns(paste0("module_save_figure_indiv_plots", i)), label = NULL) 
      })
      
    ) # tagList
  })
  
  
  ################################
  # distribution_plots_container
  ################################
  output$distribution_plots_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    # basic GOF plots
    fig_name_lst <- intersect(names(values$figure), distribution_plots_lst)
    fig_lst <- values$figure[fig_name_lst]
    validate(need(length(fig_lst)>0, message="no figure found"))
    
    tagList(
      fluidRow(
        column(12,  offset=10,
               actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
        )
      ), 
      
      lapply(1:length((fig_lst)), function(i) {
        validate(need(fig_lst[[i]], message="no figure found"), 
                 need(is.ggplot(fig_lst[[i]]) | class(fig_lst[[i]])[1]=="plotly", 
                      message="only ggplot/plotly object allowed")
        )
        
        # save fig_lst into ALL$FIGURE
        ALL = callModule(module_save_figure, paste0("module_save_figure_distribution_plots", i), 
                         ALL, 
                         figure = fig_lst[[i]], 
                         figure_index = i, 
                         figure_name = names(fig_lst[i]), 
                         figure_data = NULL
        )
        
        module_save_figure_UI(ns(paste0("module_save_figure_distribution_plots", i)), label = NULL) 
      })
      
    ) # tagList
  })
  
  
  
  ################################
  # vpc_plots_container
  ################################
  output$vpc_plots_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    # basic GOF plots
    fig_name_lst <- intersect(names(values$figure), vpc_plots_lst)
    fig_lst <- values$figure[fig_name_lst]
    validate(need(length(fig_lst)>0, message="no figure found"))
    
    tagList(
      fluidRow(
        column(12,  offset=10,
               actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
        )
      ), 
      
      lapply(1:length((fig_lst)), function(i) {
        validate(need(fig_lst[[i]], message="no figure found"), 
                 need(is.ggplot(fig_lst[[i]]) | class(fig_lst[[i]])[1]=="plotly", 
                      message="only ggplot/plotly object allowed")
        )
        
        # save fig_lst into ALL$FIGURE
        ALL = callModule(module_save_figure, paste0("module_save_figure_vpc_plots", i), 
                         ALL, 
                         figure = fig_lst[[i]], 
                         figure_index = i, 
                         figure_name = names(fig_lst[i]), 
                         figure_data = NULL
        )
        
        module_save_figure_UI(ns(paste0("module_save_figure_vpc_plots", i)), label = NULL) 
      })
      
    ) # tagList
  })
  
  
  ################################
  # other_plots_container
  ################################
  output$other_plots_container <- renderUI({  
    validate(need(is.list(values$figure), message="no figure found, or output$figure needs to be a list"))
    
    # basic GOF plots
    fig_name_lst <- intersect(names(values$figure), other_plots_lst)
    fig_lst <- values$figure[fig_name_lst]
    
    tagList(
      fluidRow(
        column(12,  offset=10,
               actionButton(ns("save_all_figure"),label="Save all", style=actionButton_style)
        )
      ), 
      
      lapply(1:length((fig_lst)), function(i) {
        validate(need(fig_lst[[i]], message="no figure found"), 
                 need(is.ggplot(fig_lst[[i]]) | class(fig_lst[[i]])[1]=="plotly", 
                      message="only ggplot/plotly object allowed")
        )
        
        # save fig_lst into ALL$FIGURE
        ALL = callModule(module_save_figure, paste0("module_save_figure_other_plots", i), 
                         ALL, 
                         figure = fig_lst[[i]], 
                         figure_index = i, 
                         figure_name = names(fig_lst[i]), 
                         figure_data = NULL
        )
        
        module_save_figure_UI(ns(paste0("module_save_figure_other_plots", i)), label = NULL) 
      })
      
    ) # tagList
  })
  
  ################################
  # run_script  
  ################################ 
  observeEvent(input$run_script, {
    validate(need(input$script_content, message="no script loaded yet")
    )
    
    ihandbook = 1
    dataset = xpdb_inputData() # 
    
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
    ALL$TABLE = c(ALL$TABLE, values$table)
    showNotification("all tables saved", type="message") 
    
  })
  
  
  observeEvent({input$save_all_data}, {
    validate(need(length(values$data), message="no data found") ) 
    
    #lapply(1:length(values$data), function(i) ALL$DATA[[names(values$data)[i]]] = values$data[[i]])
    ALL$DATA = c(ALL$DATA, values$data)
    showNotification("all data saved", type="message") 
    
  })
  
  
  observeEvent({input$save_all_figure}, {
    validate(need(length(values$figure), message="no figure found") )
    isolate({ 
      #lapply(1:length(values$figure), function(i) ALL$FIGURE[[names(values$figure)[i]]] = values$figure[[i]])
      ALL$FIGURE = c(ALL$FIGURE, values$figure)
      
      showNotification("all figures saved", type="message") 
    })
    
  })
  
  return(ALL)
}



