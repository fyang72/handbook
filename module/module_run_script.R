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
           
       # data_container
       tabPanel(width=12, title="data", value = "data", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("data_container"))))
       ),
       
       # table_container
       tabPanel(width=12, title="table", value = "table", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                #fluidRow(column(width=12, uiOutput(ns("table_container"))))
                fluidRow(column(width=12, uiOutput(ns("multiple_tables_container"))))
       ),
       
       #figure_container
       tabPanel(width=12, title="figure", value = "figure", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE, 
                fluidRow(column(width=12, uiOutput(ns("multiple_figures_container"))))   
                
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
                              ALL, dataset, script="", params=NULL
                              )  {

  # script is a list of file names containing the "script"
  script <- sapply(script, function(i) paste0(readLines(i), collapse="\n")) %>% unlist()
  
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL, table = NULL)

  ################################
  # Load dataset
  ################################ 
  load_dataset <- reactive({
    #dataset
    ALL$DATA[["mYtEsT_for_run_script"]]
    
  })
  
  filtered_dataset <- reactive({
    
    dataset = load_dataset()
    validate(need(dataset, message="no dataset"))
    
    if (class(dataset)[1] == "xpose_data") {
      #tdata = dataset %>% slot("Data")  # xpose4
      tdata = xpose::get_data(dataset)  # xpose
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
  # UI for dataset_container
  ################################ 
  output$dataset_container <-renderUI({
    
    fluidRow(
      # dataset
      HTML(colFmt("Step 1: Load the dataset<br>", color="darkblue")
      ),
      uiOutput(ns("load_dataset_container")),
      fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
      
      # filters
      HTML(colFmt("Step 2: Apply following filter(s), if needed, to narrow down the dataset<br>", color="darkblue")
      ),
      #uiOutput(ns("filter_dataset_container")), 
       fluidRow(column(width=12,
                       fluidRow(column(width=6, uiOutput(ns("select_STUDYID_container")))), 
                       fluidRow(column(width=6, uiOutput(ns("select_TEST_container")))), 
                       fluidRow(column(width=12, uiOutput(ns("select_ARMA_container"))))
       )),
      fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
      
      #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
  
      HTML(colFmt("Step 3: Review the (filtered) dataset <br>", color="darkblue")
      ),
      uiOutput(ns("render_filtered_dataset_container")), 
      style='margin-bottom:30px;  border:1px solid; padding: 10px;'
   
    ) 
    
  })
  
  output$load_dataset_container <-renderUI({
    
    # callModule 
    ALL = callModule(module_load_dataset, "load_dataset_for_run_script", 
                     ALL, dataset_name="mYtEsT_for_run_script")
    
    # UI  
    fluidRow(column(6, 
                    module_load_dataset_UI(id=ns("load_dataset_for_run_script"), label=NULL) 
                    ), 
              column(6, 
                   HTML(colFmt("internal library: build-in dataset <br>
                        within session: secondary data derived from the original <br> 
                        external file: external file", color="gray")
                   )
             )
    )
   
  })
  
  
  output$filter_dataset_container <-renderUI({
    
    # callModule 
    values$filtered_dataset2 = callModule(module_filtered_dataset, "load_dataset_for_run_script", 
                     load_dataset()
                     )
     
     
    module_filtered_dataset_UI(id=ns("load_dataset_for_run_script"), label=NULL) 
    
    
  })
  
  
  
  
  
  output$select_STUDYID_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE),  
             need("STUDYID" %in% colnames(tdata), message=FALSE) 
    )
    
    studyid_lst = c(unique(tdata%>%drop_na(STUDYID)%>%pull(STUDYID))) 
    validate(need(length(studyid_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    # column(6,
    inline(
      selectizeInput(ns("which_studyid"), 
                     label    = "select which study", 
                     choices  = studyid_lst, 
                     multiple = TRUE,
                     width="100%", 
                     selected = studyid_lst[1]
      )
    )
    # )
   # )
    
  })
  
  
  output$select_TEST_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE),  
             need("TEST" %in% colnames(tdata), message=FALSE) 
    )
    
    test_lst = c(unique(tdata%>%drop_na(TEST)%>%pull(TEST))) 
    validate(need(length(test_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    # column(6,
    inline(
      selectizeInput(ns("which_test"), 
                     label    = "select which analyte", 
                     choices  = test_lst, 
                     multiple = FALSE,
                     width="100%", 
                     selected = test_lst[1]
      )
    )
    # )
    # )
    
  })
  
  
  output$select_ARMA_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE), 
             need("ARMA" %in% colnames(tdata), message=FALSE) 
    )
    
    arma_lst = c(unique(tdata%>%drop_na(ARMA)%>%pull(ARMA)))
    validate(need(length(arma_lst)>1, message=FALSE)) 
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
   # fluidRow(
    #column(8, 
    inline(
      selectizeInput(ns("which_arma"), 
                     label    = "select which dose group(s)", 
                     choices  = arma_lst, 
                     multiple = TRUE,
                     width="100%", 
                     selected = arma_lst
      )
    )
    #)
   # )
  })
  
  
  output$render_filtered_dataset_container <- renderUI({  
    
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
    br(),
    
    fluidRow(
      column(2,  offset=6,
             actionButton(ns("save_all_table"),label="Save all", style=actionButton_style)
      ),
      column(2,  #offset=10,
             downloadButton(ns("docx_all_table"),label="docx all", icon=icon("download"), style=actionButton_style)
      ),
      column(2,  #offset=10,
             downloadButton(ns("pptx_all_table"),label="pptx all", icon=icon("download"), style=actionButton_style)
      )
    ), 
    
    br(),
    
    
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
# run_script  
################################ 
observeEvent(input$run_script, {
  validate(need(input$script_content, message="no script loaded yet")
  )

  ihandbook = 1
  dataset = filtered_dataset() # 
  
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
observeEvent({input$save_all_data}, {
  validate(need(length(values$data), message="no data found") ) 
 
  #lapply(1:length(values$data), function(i) ALL$DATA[[names(values$data)[i]]] = values$data[[i]])
  ALL$DATA = c(ALL$DATA, values$data)
  showNotification("all data saved", type="message") 
 
})





####################################################################
# save all tables
####################################################################
output$multiple_tables_container <-renderUI({
  
  # callModule 
  ALL = callModule(
    module_save_multiple_tables, "multiple_tables", ALL, values$table
  ) 
  
  module_save_multiple_tables_UI(id=ns("multiple_tables"), label=NULL) 
  
})


# save_all_table
#------------------
observeEvent({input$save_all_table}, {
  validate(need(length(values$table), message="no table found") )
  
  #lapply(1:length(values$table), function(i) ALL$TABLE[[names(values$table)[i]]] = values$table[[i]])
  ALL$TABLE = c(ALL$TABLE, values$table)
  showNotification("all tables saved", type="message") 
  
})

# docx_all_table
#------------------
output$docx_all_table <- downloadHandler(
  filename = function() {     
    paste0("output", ".docx")                                                                                                                                                                       
  },
  
  content = function(file) {
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    #owd <- setwd(tempdir())
    #on.exit(setwd(owd))
    #mydoc <-docx()    # D$documents[[1]]
    
    #myfig <- NULL
    #mytabl <- values$table   #[[input$figure_name]] <- ggplot_figure()  
    
    #tt <- print2_word_ppt2(FIGURE_ALL=NULL,  TABLE_ALL=mytabl, mydoc=NULL, myppt=NULL)
    # mydocx <- docx() 
     
    
    mydocx <- read_docx(paste0(HANDBOOK_HOME, "/lib/docTemplate.docx")) %>% 
      print2docx(FIGURE=NULL, TABLE=values$table)
    
    validate(need(!is.null(mydocx), "no docx found"))
    
    #writeDoc(mydocx, file)
    print(mydocx, target = file)
    
  })

# pptx_all_table
#------------------
output$pptx_all_table <- downloadHandler(
  
  filename = function() { 
    paste0("output", ".pptx")    
  },
  
  content = function(file) {
    #if (is.null(inputData()))  {return(NULL)   }
    
    #tmpdir <- setwd(tempdir())
    #on.exit(setwd(tmpdir))
    
    #myppt <-pptx()    
    #myfig <- NULL
    #myfig[[input$figure_name]] <- ggplot_figure() 
    #mytabl <- values$table
    
    #tt <- print2_word_ppt2(FIGURE_ALL=NULL, TABLE_ALL=mytabl, mydoc=NULL, myppt=NULL)
    mypptx <- read_pptx(paste0(HANDBOOK_HOME, "/lib/pptTemplate.pptx")) %>% 
      print2pptx(TABLE=values$table)
    
    validate(need(!is.null(mypptx), "no pptx found"))
    
    #writeDoc(mypptx,file=file)
    print(mypptx, target = file)
    
    # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
    #zip(zipfile=file, files=c(fileDOC, filePPT) )
    #browseURL(fileDOC)
  }
  #contentType = "application/zip"
  #contentType="application/octet-stream"  #ms-word
  #contentType="application/ms-word"  #ms-word
)

####################################################################
# save all figures
####################################################################
 

output$multiple_figures_container <-renderUI({
  
  # callModule 
  ALL = callModule(
    module_save_multiple_figures, "multiple_figures", ALL, values$figure
  ) 
  
  module_save_multiple_figures_UI(id=ns("multiple_figures"), label=NULL) 
  
})

  
return(ALL)
}



