 
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

module_review_checkout_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  #tagList( 
              #h5("Number of figures:"),
      # fluidRow(
      #   column(6, textOutput(ns("finalFigCount"))), 
      #   column(6, textOutput(ns("finalTabCount")))
      # ),  
      
      fluidRow(
        column(6, #style='padding:0px;', 
               fileInput(ns("docx_template"), label = "load your docx template", 
                         accept=c('.docx') #https://www.sitepoint.com/mime-types-complete-list/
                         ), 
               
               fileInput(ns("pptx_template"), label = "load your pptx template", 
                         accept=c('.pptx')   
                         ) 
               
               
        ), 
        column(6, #style='padding:0px;', 
               fluidRow(
                 column(12,downloadButton(ns("download_docx"), 
                              label="Download docx", 
                              icon=icon("download"),
                              style = "margin-top: 25px;",
                              style=actionButton_style 
               ))),  
               
               fluidRow(
                 column(12,downloadButton(ns("download_pptx"), 
                              label="Download pptx", 
                              icon=icon("download"),
                              style = "margin-top: 65px;",
                              style=actionButton_style 
               )))
        )
      )
 
        
   # )
  
}

###########################################
# module_check_out
###########################################
# Module server function
module_review_checkout <- function(input, output, session, ALL) {
  
  ns <- session$ns
   
  #----------------------------------------------
  # server side [Checkout]
  #----------------------------------------------
  output$finalFigCount <- renderText(
    paste0("Number of figures:", length(ALL$FIGURE))
  )

  output$finalTabCount <- renderText(
    paste0("Number of tables:", length(ALL$TABLE))
  )
 
   
  #------------------------
  # load doc template 
  #------------------------
  docx_input <- reactive({
    #req(input$docx_template)
    #validate(need(input$docx_template, message = FALSE))
    
    library(officer)
    if(is.null(input$docx_template)) {
      mydocx <- read_docx(path= paste0(HOME, '/lib/docTemplate.docx'))
    }else{
      inFile = input$docx_template   # docx_template
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ext, sep="."))
      mydocx <- read_docx(path=paste(inFile$datapath, ext, sep="."))
    }
    
    mydocx
  })   
  
  #------------------------
  # load ppt template
  #------------------------
  library(officer)
  pptx_input <- reactive({
    #req(input$pptx_template)
    #validate(need(input$pptx_template, message = FALSE))
    
    if(is.null(input$pptx_template)) {
      mypptx <- read_pptx(path = paste0(HOME, '/lib/pptTemplate_long_format.pptx'))
       
    }else{
      inFile = input$pptx_template
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ext, sep="."))
      
      mypptx <- read_pptx(path = paste(inFile$datapath, ext, sep="."))
    }
    
    mypptx
  })    
   
  #----------------------------------------------
  # generate a report
  #----------------------------------------------
  #observe({
  D <- reactiveValues(documents = NULL)
  
  observeEvent(input$GenerateReport, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Building Content...Please Wait", value = 0)
 
    # doc/ppt template 
    mydoc <- docInput()   # sort(list_bookmarks(mydoc))
    myppt= pptInput()    # slide.layouts(myppt)
      
    tt <- print2_word_ppt(ALL$FIGURE, ALL$TABLE, mydoc, myppt)
    # construct a word document based on the constructed doc object
    #writeDoc(doc,"Report2.docx")
    
    # open result
    #browseURL("Report2.docx")
    
    ###Create list of reactive object
    D$documents <- tt  #list(doc, ppt)
  })
  
  
  
  
  output$download_docx <- downloadHandler(
    filename = function() {
      paste('my-report.docx') 
    },
    
    content = function(file) {
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      
      # Create a Progress object
       progress <- shiny::Progress$new()
      # # Make sure it closes when we exit this reactive, even if there's an error
       on.exit(progress$close())
       progress$set(message = "Building Content...Please Wait", value = 0)
   
      #mydocx <-   #read_docx(path= 'C://Users//feng.yang//Documents//handbook//lib//docTemplate.docx')
      mydocx <- docx_input() %>% print2docx(
        ALL$FIGURE, 
        ALL$TABLE, 
        ALL$DATA[["BODY_REPLACE_TEXT_AT_BKM"]],   
        ALL$DATA[["BODY_REPLACE_ALL_TEXT"]] 
        )  # docx_input()
      #fileDOC = mydocx %>% print(target = "~/handbook/tmp.docx") 
      #browseURL(fileDOC)
       
      mydocx %>% print(target = file) 
    }#, 
    #contentType = "application/zip"
    #contentType = "application/octet-stream"  #ms-word
    #contentType = "application/ms-word"  #ms-word
  )
  
  output$download_pptx <- downloadHandler(
    filename = function() {
      paste('my-report.pptx') 
    },
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Building Content...Please Wait", value = 0)

      #if (file.exists(file)) {file.remove(file)} #Delete file if it exists
      #pptx_input() %>% 
      print2pptx(mypptx=NULL, FIGURE=ALL$FIGURE, TABLE=ALL$TABLE) %>% print(target = file) 
      
      if (1==2)  {
        # doc/ppt template  
        environment(try_eval) <- environment()
        env = try_eval(text="mypptx <- pptx_input() %>% print2pptx(ALL$FIGURE, ALL$TABLE)")
        
        output=NULL; error_message=NULL
        if ("mypptx" %in% ls(env)) {mypptx = get("mypptx", env)}
        if ("message" %in% ls(env)) {error_message = get("message", env)}
        
        #removeNotification("myid")
        if ((length(error_message)==0 & !is.null(mypptx))) {
          showNotification("Printing...", type="message", id="myid") 
          mypptx %>% print(target = file) 
  
        }else{
          showNotification(paste0(error_message, collapse="\n"), type="error", id="myid")
          NULL
        }   
        
      }
      # https://groups.google.com/forum/#!topic/shiny-discuss/zATYJCdSTwk
      # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
      #zip(zipfile=file, files=c(fileDOC, filePPT) )
      #browseURL(fileDOC)
      
    }#, 
    #contentType = "application/zip"
    #contentType = "application/octet-stream"  #ms-word
    #contentType = "application/ms-word"  #ms-word
  )
  
  
  
  
  ###################ReporteRs Download Handler Word####################################################################
  # output$download_doc2 <- downloadHandler(
  #   filename = "handbook_doc_output.docx",
  #   content = function(file) {
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     doc <- D$documents[[1]]
  #     validate(
  #       need(!is.null(doc), "You have to press Build Network Content First and let it run, usually takes 1-2 minutes.")
  #     )
  #     writeDoc(doc,file)
  #   })
   
  output$download_doc <- downloadHandler(
    
    filename = function() { 
      #data = inputData()
      #studyid = unique(as.character(data$STUDYID))[1]
      paste("PMx-KRM-report.docx", sep="")
    },
    
    content = function(file) {
      #if (is.null(inputData()))  {return(NULL)   }
      
      tmpdir <- setwd(tempdir())
      on.exit(setwd(tmpdir))
      
      mydoc <- D$documents[[1]]
      validate(
        need(!is.null(mydoc), "You have to press 'Generate it' First and let it run, usually takes a few seconds.")
      )
      
      writeDoc(mydoc,file=file)
      # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
      #zip(zipfile=file, files=c(fileDOC, filePPT) )
      #browseURL(fileDOC)
      
    }
    #contentType = "application/zip"
    #contentType="application/octet-stream"  #ms-word
    #contentType="application/ms-word"  #ms-word
  ) 
  
  
  output$download_ppt <- downloadHandler(
    
    filename = function() { 
      #data = inputData()
      #studyid = unique(as.character(data$STUDYID))[1]
      paste("PMx-KRM.pptx", sep="")
      
    },
    
    content = function(file) {
      
      tmpdir <- setwd(tempdir())
      on.exit(setwd(tmpdir))
      
      myppt <- D$documents[[2]]
      validate(
        need(!is.null(myppt), "You have to press 'Generate It' First and let it run, usually takes a few seconds.")
      )
      
      #writeDoc(doc, file="./doc/tmp.pptx")
      writeDoc(myppt,file=file)
      
      # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
      #zip(zipfile=file, files=c(fileDOC, filePPT) )
      #browseURL(fileDOC)
      
    }
    #contentType = "application/zip"
    #contentType="application/octet-stream"  #ms-word
    #contentType="application/ms-word"  #ms-word
  ) 
  
  # 
  # # We can run observers in here if we want to
  # observe({
  #   msg <- sprintf("figure review accordingly.")
  #   cat(msg, "\n")
  # })
  
  
  # Return the reactive that yields the data frame
  #return(inputData)
  # return all input as a list?
}
