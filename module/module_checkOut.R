 
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

checkOutUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
  
    column(6,              
        
         h5("Number of figures:"),
         h2(textOutput(ns("finalFigCount"))),

         h5("Number of tables:"),
         h2(textOutput(ns("finalTabCount")))

         # fluidRow(
         #   column(width=12, valueBoxOutput(ns("finalFigCount")))),
         # 
         #   fluidRow(
         #   column(width=12, valueBoxOutput(ns("finalTabCount")))
         # )
    ), 
       column(width=6,    
        
              fluidRow(uiOutput(ns("loadTemplate_container"))),
           
              fluidRow(
                column(width=4, offset=4, actionButton(ns("GenerateReport"),"Generate it", 
                             style=actionButton_style)
                )
              ),   # "primary",
              
              fluidRow(
                column(width=6, 
                  downloadButton(ns("download_doc"), 
                                 label="Download docx", 
                                 icon=icon("download"), 
                                 style=actionButton_style )
                  ),   #, icon("download")
                
                column(width=6,
                  downloadButton(ns("download_ppt"), 
                                 label="Download pptx", 
                                 icon=icon("download"), 
                                 style=actionButton_style )
                  )   # icon("paper-plane")
              )
       )
    )
  
}

###########################################
# checkOut
###########################################
# Module server function
checkOut <- function(input, output, session, ALL) {
  
  ns <- session$ns
 
  output$loadTemplate_container <- renderUI({
    
    fluidRow(
      column(6,
             fileInput(ns("doc_template"), label = h5("load your docx template"), 
                       accept=c('.docx'))),
      
      column(6,
             fileInput(ns("ppt_template"), label = h5("load your pptx template"), 
                       accept=c('.pptx')))   
    )
    
  })
  
  
  #----------------------------------------------
  # server side [Checkout]
  #----------------------------------------------
  output$finalFigCount <- renderText(
    length(ALL$FIGURE) 
  )

  output$finalTabCount <- renderText(
    length(ALL$TABLE) 
  )
 
   
  #------------------------
  # load doc template 
  #------------------------
  docInput <- reactive({
    #req(input$doc_template)
    #validate(need(input$doc_template, message = FALSE))
    
    library(ReporteRs)
    if(is.null(input$doc_template)) {
      mydoc <- docx(template = './lib/docTemplate.docx', empty_template = FALSE)
    }else{
      inFile = input$doc_template   # doc_template
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ext, sep="."))
      
      mydoc <- docx(template = paste(inFile$datapath, ext, sep="."), empty_template = FALSE)
    }
    
    mydoc
  })   
  
  #------------------------
  # load ppt template
  #------------------------
  library(ReporteRs)
  pptInput <- reactive({
    #req(input$ppt_template)
    #validate(need(input$ppt_template, message = FALSE))
    
    if(is.null(input$ppt_template)) {
      myppt<- pptx(title=paste0("Key Results for STUDYID"), 
                   template = './lib/pptTemplate.pptx')
       
    }else{
      inFile = input$ppt_template
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ext, sep="."))
      
      myppt <- pptx(title = "title", template = paste(inFile$datapath, ext, sep="."))
    }
    
    myppt
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
