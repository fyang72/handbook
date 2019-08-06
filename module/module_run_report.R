

#Tips and tricks for working with images and figures in R Markdown documents
# http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
  
  
  #-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------
module_run_report_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
#     fluidRow(
#       column(width=12,  
#              HTML(colFmt("Note, the following tabset is used to 
# 1) select dataset (dataset tab), 
# 2) select report template or modify it if needed, select report parameters if any, finally download the report (script tab), 
# 3) render the derived data (data tab), final table (table tab) and figure (figure tab) during report generating.", 
#                          color="gray")))
#       ),
    
    tabBox(width=12, id = ns("run_script_for_data_figure_table"), title =NULL, 
           
           # dataset_container 
           tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("dataset_container"))))
           ),       
           
           # script_container 
           tabPanel(width=12, title="template", value = "template", collapsible = TRUE, 
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

module_run_report <- function(input, output, session, 
                              ALL, key="report_template_pk",  params=NULL
)  {
  
  # script is a list of file names containing the "script"
  # scripts
  script <- list.files(path=paste0(HOME, "/script/"), 
                       full.names = TRUE,
                       pattern=key)
  names(script) = basename(script)
  
  script <- sapply(script, function(i) paste0(readLines(i), collapse="\n")) %>% unlist()
  
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL, table = NULL)
 

  ################################
  # UI for dataset_container
  ################################ 
  output$dataset_container <-renderUI({
    
      fluidRow(
        # dataset
        HTML(colFmt("Step 1: Load the dataset<br>", color="darkblue")
        ),
        uiOutput(ns("pk_report_data_selector")),
        #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
        
        # filters
        HTML(colFmt("Step 2: Apply following filter(s), if needed, to narrow down the dataset<br>", color="darkblue")
        ),
        uiOutput(ns("select_STUDYID_container")), 
        uiOutput(ns("select_TEST_container")), 
        uiOutput(ns("select_ARMA_container")),
        #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
        
        #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
        
        # ),
        # fluidRow(
        #   column(12, 
        HTML(colFmt("Step 3: Review the (filtered) dataset <br>", color="darkblue")
        ),
        uiOutput(ns("filtered_dataset_container")), 
        style='margin-bottom:30px;  border:1px solid; padding: 10px;'
        #)
      ) 
  
  })
  
  output$pk_report_data_selector <-renderUI({
    
    # callModule 
    ALL = callModule(module_load_dataset, "load_nmdat_for_pk_report", 
                     ALL, dataset_name="mYtEsT_for_pk_report")
     
    # UI  
    fluidRow(column(6, 
                    module_load_dataset_UI(id=ns("load_nmdat_for_pk_report"), label=NULL)
    )
    )  
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
    tdata = load_dataset()
    validate(need(tdata, message=FALSE), 
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
  
  
  output$filtered_dataset_container <- renderUI({  
    
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
  # UI for rmd_content_container
  ################################
  output$script_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    script_lst <- c("", names(script))
    tagList(
      
      fluidRow(
        column(6, 
            HTML(colFmt("Step 1: select which script template<br>", color="darkblue")
            ),
            
            fluidRow(
              column(12,
                     selectizeInput(ns("script_selector"),
                                    label    = NULL, #"select script:" ,
                                    choices  = script_lst,
                                    multiple = FALSE,
                                    selected = script_lst[1]
                     )
              )
              # column(3,
              #        actionButton(ns("run_script"), label="Run script", style=actionButton_style )
              # ) 
            ), 
            
            # download_report_container
            #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
            HTML(colFmt("Step 2: choose the options provided, then download it <br>", color="darkblue")
            ),
            fluidRow(column(12, uiOutput(ns("download_report_container"))))
        ), 
        
        column(6, uiOutput(ns("ppt_word_checkout_container")))
        ), 
      
      
      
      
      # rmd_content_container
      #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
      HTML(colFmt("Step 3: optionally, modify the script, then re-download it.<br>", color="darkblue")
      ),
      fluidRow(column(12, uiOutput(ns("rmd_content_container"))))
      
      )
      
    
  })
  
  
  ################################
  # UI for rmd_content_container
  ################################ 
output$rmd_content_container <- renderUI({     
      
  validate(need(input$script_selector, message=FALSE))
  
    aceEditor(ns("rmd_content"),
              mode="r",
              value= if (is.null(names(script))) {script} else {script[input$script_selector]},
              theme = "crimson_editor",   # chrome
              autoComplete = "enabled",
              height = "1000px",
              fontSize = 15
    )
    
  })
  
  
  ################################
  # UI for download_report_container
  ################################
  
  
  output$download_report_container <- renderUI({ 
    
    validate(need(input$script_selector, message=FALSE))
     
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, 
            PDF = 'pdf', 
            HTML = 'html', 
            Word = 'docx' 
            #Template_Word='docx'
        ))
      },
      
      content = function(file) {
        #src <- normalizePath(paste0(HOME, "/script/report_template_pk.Rmd"))
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        #file.copy(src, 'report_template.Rmd', overwrite = TRUE)
        
        fileConn<-file("report_template.Rmd")
        writeLines(input$rmd_content, fileConn)
        close(fileConn)
        
        library(rmarkdown)
        
        #out <- render_report("report_template.Rmd", input, regFormula, 'sf', 134)
        out <- rmarkdown::render(
        #out <- bookdown::render_book(
          input = 'report_template.Rmd', 
          params = list(
            dataset = filtered_dataset(), 
            title = "test title", 
            subtitle = "testing subtitle",
            region = "Europe",
            year = 2019
          ),
          output_format = switch(
            input$format,
            PDF = pdf_document(), 
            HTML = html_document(), 
            Word = word_document()
          ))
        
        #if (input$format %in% c('pdf', 'html', 'docx'))  {
            file.rename(out, file)
         # }else if (input$format %in% c('pptx'))  {
          #  mypptx=NULL
          #  mypptx %>% print2_pptx(FIGURE=values$figure, TABLE=values$table) %>% 
          #    print(target = file) 
          # }
      
      }
    )
    
     
    
    
    # UI
    fluidRow(
      column(12, 
             helpText(), 
              #selectInput(ns('x'), 'Build a regression model of mpg against:',
               #           choices = names(mtcars)[-1]),
              
             radioButtons(ns('format'), 
                          label='Document format', 
                          choices= c('PDF', 'HTML', 'Word'),
                          inline = TRUE),
             downloadButton(ns('downloadReport'), label="Download", style=actionButton_style)#,
              
      ) 
    ) 
    
  })
  
  
  output$ppt_word_checkout_container <- renderUI({ 
    
    #validate(need(drug_descriptive_analysis_inputData(), message="no data found yet"))
    validate(need(!is.null(values$table) | 
                  !is.null(values$figure), 
                  message="no table or figure found"))
    
    values2 = values
    values2$TABLE = values$table
    values2$FIGURE = values$figure
    
    # callModule
    callModule(module_review_checkout, "mycheckOut", values2)  
    #, dataset, script, params=NULL
    
    # UI
    module_review_checkout_UI(ns("mycheckOut"), label="") 
    
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
  
  
  
  
  load_dataset <- reactive({
     ALL$DATA[["mYtEsT_for_pk_report"]]
     
  })
  
  
  filtered_dataset <- reactive({
    
    dataset = load_dataset()
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
    validate(need(input$rmd_content, message="no script loaded yet")
    )
    
    ihandbook = 1
    
    environment(try_eval) <- environment()
    env = try_eval(text=input$rmd_content)
    
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



