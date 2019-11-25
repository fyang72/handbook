
################################################################################ 
# module_save_multiple_tables_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_save_multiple_tables_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  tagList( 
    
    fluidRow(column(12, uiOutput(ns("checkboxGroup_input")))),
    
    fluidRow(
      column(2,  
             actionButton(ns("update_all_tables"),label="Refresh tables", style=actionButton_style)
      ),
      
      column(2,  offset=4,
             actionButton(ns("save_all_tables"),label="Save all", style=actionButton_style)
      ),
      column(2,  #offset=10,
             downloadButton(ns("docx_all_tables"),label="docx all", icon=icon("download"), style=actionButton_style)
      ),
      column(2,  #offset=10,
             downloadButton(ns("pptx_all_tables"),label="pptx all", icon=icon("download"), style=actionButton_style)
      )
    ), 
    
    br(),
    
    uiOutput(ns("multiple_tables_container"))
  )
}


################################################################################ 
# module_save_multiple_tables
################################################################################

module_save_multiple_tables <- function(input, output, session, 
                                        ALL, tables=NULL 
){
  
  ns <- session$ns 
  values <- reactiveValues(tables=tables)
  
  
  output$checkboxGroup_input <- renderUI({  
    shiny::validate(need(tables, message="no table found yet"))
    
    checkboxGroupInput(ns("table_list"), "Select table(s):",
                       inline=TRUE, 
                       choices = names(tables), 
                       selected = names(tables)
    )
    
  })
  
  
  output$multiple_tables_container <- renderUI({  
    validate(need(is.list(tables), message="no table found, or output$table needs to be a list"))
    
    subset_of_tables <- values$tables  #figures[input$figure_list]
    
    lapply(1:length((subset_of_tables)), function(i) {
      validate(need(subset_of_tables[[i]], message="no table found"), 
               need(is.data.frame(subset_of_tables[[i]]), message="only data.frame object allowed")
      )
      
      # save table into ALL$table
      ALL = callModule(module_save_table, paste0("module_save_table_", i), 
                       ALL, 
                       table = subset_of_tables[[i]], 
                       table_index = i, 
                       table_name = names(subset_of_tables[i])
      )
      
      module_save_table_UI(ns(paste0("module_save_table_", i)), label = NULL) 
    })
    
  })
  
  
  # update_all_tables
  #------------------
  observeEvent({input$update_all_tables}, {
    validate(need(length(tables), message="no figure found"), 
             need(input$table_list, message=FALSE)
    )
    
    values$tables = tables[input$table_list]
    
    showNotification("all tables updated", type="message") 
    
    
  })
  # save_all_tables
  #------------------
  observeEvent({input$save_all_tables}, {
    validate(need(length(values$tables), message="no table found") )
    isolate({ 
      
      #lapply(1:length(values$table), function(i) ALL$table[[names(values$table)[i]]] = values$table[[i]])
      ALL$TABLE = c(ALL$TABLE, values$tables)
      
      showNotification("all tables saved", type="message") 
    })
    
  })
  
  # docx_all_tables
  #------------------
  output$docx_all_tables <- downloadHandler(
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
      #myfig <- values$table   #[[input$table_name]] <- ggplot_table()  
      
      #tt <- print2_word_ppt2(myfig, TABLE_ALL=NULL, mydoc=NULL, myppt=NULL)
      mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx"))  %>% 
        print2docx(TABLE=values$tables)
      
      validate(need(!is.null(mydocx), "no doc found"))
      print(mydocx, target =file)
      #writeDoc(mydocx, file)
    })
  
  # pptx_all_tables
  #------------------
  output$pptx_all_tables <- downloadHandler(
    
    filename = function() { 
      paste0("output", ".pptx")    
    },
    
    content = function(file) {
      #if (is.null(inputData()))  {return(NULL)   }
      
      #tmpdir <- setwd(tempdir())
      #on.exit(setwd(tmpdir))
      
      #myppt <-pptx()    
      #myfig <- NULL
      #myfig[[input$table_name]] <- ggplot_table() 
      #myfig <- values$table
      
      #tt <- print2_word_ppt2(myfig, TABLE_ALL=NULL, mydoc=NULL, myppt=NULL)
      #mypptx <- pptx() %>% print2docx(table=values$table)
      mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate.pptx")) 
      mypptx <- mypptx %>% print2pptx(TABLE=values$tables)
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
  
  return(ALL)
}




################################################################################ 
# module_save_table_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_save_table_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  validate(need(globalVars$login$status, message=FALSE)
  )
  
  fluidRow(  
    
    fluidRow(
      column(12, uiOutput(ns("table_title_container")))
      ), 
           
    fluidRow(
      column(width=12,  
             uiOutput(ns("table_container"))
      )
    ), 
    
    fluidRow(
      column(width=6, #status = "primary",  #class = 'rightAlign',#background ="aqua",
             uiOutput(ns("table_name_container"))  
      ), 
      
      column(2,  
             actionButton(ns("saveit"),label="Save it", style=actionButton_style)
      ) , 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloaddoc"),label="docx", icon=icon("download"), style=actionButton_style)                                                                        
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloadppt"),label="pptx", icon=icon("download"), style=actionButton_style)                                                                        
      )
      
    ), 
    style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;")))
    # fluidRow(column(width=12, tags$hr(
    #   style="display: block;
    #   height: 1px;
    #   border: 0;
    #   border-top: 1px solid #ccc;
    #   margin: 1em 0;
    #   padding: 0; "
    # )))
  )
} 




################################################################################ 
# main function: module_save_table
################################################################################

module_save_table <- function(input, output, session, ALL, 
                              table, 
                              table_index = 1,   # in values$table[[1]]
                              table_name = "table_name"    # values$table[["table_name]]
                             #default_table_name = "tab-"#,
                             # default_data_name = "data-" #, 
                              #data_name= "" 
) {
  
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton_style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  output$table_title_container <- renderUI(renderText({
      paste0("<b>Table ", table_index, ": ", attributes(table)$title,"</b>")
  })() %>% HTML()
  
  )
  
  
  
  
  
  #--------------------------------------  
  # table_name_container
  #-------------------------------------- 
  output$table_name_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))  
      
    textInput(ns("table_name"), value=table_name, width="100%", label=NULL)
  })
  
   
  #--------------------------------------  
  # table_container # https://shiny.rstudio.com/articles/render-table.html
  #-------------------------------------- 
  output$table_container <- renderUI({
    #validate(need(table, message="no table found"))
    
    #callModule(module_table_output, "tab4output",  mytab=table)
    #module_table_output_UI(ns("tab4output"), label = "tab4output")
     
    output$table_output <- renderTable(table, na = "")
    tableOutput(ns('table_output'))
    
  })
  
  
  #-------------------------------------------------------------
  # save table 
  #------------------------------------------------------------- 
  observeEvent(input$saveit, {
    
    validate(need(input$saveit, message=FALSE),  
             need(table, message=FALSE)
    )
       
    print("Save table sucessfully")
    ALL$TABLE[[input$table_name]] <- table 
       
  })
  
  
  
  output$downloaddoc <- downloadHandler(
    filename = function() {     
      paste0(input$table_name, ".docx")                                                                                                                                                                       
    },
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      #mydoc <-docx()    # D$documents[[1]]
      
      #myfig <- ALL$FIGURE[input$figure_name]
      # mytab <- NULL
      # mytab[[input$table_name]] <- table  # ALL$FIGURE[[input$figure_name]]
      # 
      # tt <- print2_word_ppt(FIGURE_ALL=NULL, TABLE_ALL=mytab,   
      #                       mydoc=NULL, myppt=NULL, 
      #                       width_default=6.4,     # 8
      #                       height_default=4.8,    # 6
      #                       fontsize_default=12, 
      #                       title_default = "Type in title"
      # )
      # 
      # 
      # validate(
      #   need(!is.null(tt$mydoc), "no doc found")
      # )
      # writeDoc(tt$mydoc,file)
      
      mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx")) %>% 
        print2docx(TABLE=list("mytab"=table))
      
      validate(need(!is.null(mydocx), "no docx found"))
      
      #writeDoc(mydocx, file)
      print(mydocx, target = file)
      
      
      
      
      
    })
  
  
  
  
  output$downloadppt <- downloadHandler(
    
    filename = function() { 
      paste0(input$table_name, ".pptx")    
    },
    
    content = function(file) {
      #if (is.null(inputData()))  {return(NULL)   }
      
      #tmpdir <- setwd(tempdir())
      #on.exit(setwd(tmpdir))
      
      #myppt <-pptx()    
      
      # mytab <- NULL
      # mytab[[input$table_name]] <- table  # ALL$FIGURE[[input$figure_name]]
      # 
      # tt <- print2_word_ppt(FIGURE_ALL=NULL, TABLE_ALL=mytab,    
      #                       mydoc=NULL, myppt=NULL, 
      #                       width_default=6.4,     # 8
      #                       height_default=4.8,    # 6
      #                       fontsize_default=12, 
      #                       title_default = "Type in title"
      # )
      # 
      # validate(
      #   need(!is.null(tt$myppt), "no pptx found")
      # )
      # 
      # writeDoc(tt$myppt,file=file)
      mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate.pptx")) %>% 
        print2pptx(TABLE=list("mytab"=table))
        
      validate(need(!is.null(mypptx), "no docx found"))
      
      #writeDoc(mydocx, file)
      print(mypptx, target = file)
      
      
      # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
      #zip(zipfile=file, files=c(fileDOC, filePPT) )
      #browseURL(fileDOC)
    }
    #contentType = "application/zip"
    #contentType="application/octet-stream"  #ms-word
    #contentType="application/ms-word"  #ms-word
  ) 
  
  
  
  return(ALL)
}

