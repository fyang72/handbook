
################################################################################ 
# module_load_cppModel_UI
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
             actionButton(ns("saveit"),label="Save it", style=actionButton.style)
      ) , 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloaddoc"),label="doc", icon=icon("download"), style=actionButton.style)                                                                        
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloadppt"),label="ppt", icon=icon("download"), style=actionButton.style)                                                                        
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

module_save_table <- function(input, output, session, ALL, table, 
                              table_index = 1,   # in values$table[[1]]
                              table_name = "table_name"    # values$table[["table_name]]
                             #default_table_name = "tab-"#,
                             # default_data_name = "data-" #, 
                              #data_name= "" 
) {
  
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  output$table_title_container <- renderUI(renderText({
      paste0("Table ", table_index, " ", attributes(table)$title)
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
      mytab <- NULL
      mytab[[input$table_name]] <- table  # ALL$FIGURE[[input$figure_name]]
      
      tt <- print2_word_ppt(FIGURE_ALL=NULL, TABLE_ALL=mytab,   
                            mydoc=NULL, myppt=NULL, 
                            width_default=6.4,     # 8
                            height_default=4.8,    # 6
                            fontsize_default=12, 
                            title_default = "Type in title"
      )
      
      
      validate(
        need(!is.null(tt$mydoc), "no doc found")
      )
      writeDoc(tt$mydoc,file)
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
      
      mytab <- NULL
      mytab[[input$table_name]] <- table  # ALL$FIGURE[[input$figure_name]]
      
      tt <- print2_word_ppt(FIGURE_ALL=NULL, TABLE_ALL=mytab,    
                            mydoc=NULL, myppt=NULL, 
                            width_default=6.4,     # 8
                            height_default=4.8,    # 6
                            fontsize_default=12, 
                            title_default = "Type in title"
      )
      
      validate(
        need(!is.null(tt$myppt), "no pptx found")
      )
      
      writeDoc(tt$myppt,file=file)
      
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

