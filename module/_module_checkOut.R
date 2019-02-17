 
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

checkOutUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  
  fluidRow(
    column(6, 
           fluidRow(
             box(width=12, title="Download dataset", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 
                 fluidRow(
                   column=12, valueBoxOutput(ns("finalDataCount"))),
                 
                 fluidRow(
                    column(6, 
                              uiOutput(ns("dataset2download_Selector")),
                              downloadButton(ns("downloadData"), label="Download", style=actionButton.style)
                       )) 
                # column(6,
                        # x-variable, y-variable, and group_by
                       # tags$hr(style="border-color: black;")
                  #      
                 ) # box
             
             
             
           ) # end of fluidRow
    ),  # end of left column
    
    column(6,             
                fluidRow(
                 box(width=12, title="Download doc/ppt", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 # column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                 #        textInput(ns("tab_name"), value="Tab-", label=NULL,  
                 #                  placeholder = "Enter data name here:")),
        
                 fluidRow(column(width=12, uiOutput(ns("loadTemplate_container")))),
                 
                 # h6("Number of figures currently ordered"),
                 # h1(textOutput(ns("finalFigCount"))),
                 # 
                 # h6("Number of tables currently ordered"),
                 # h1(textOutput(ns("finalTabCount"))), 
                 
                 
                 fluidRow(
                   column(width=12, valueBoxOutput(ns("finalFigCount")))),
                 
                   fluidRow(
                   column(width=12, valueBoxOutput(ns("finalTabCount")))
                 ),
                 
                  
                 fluidRow(
                 
                   #downloadButton('downloadData', 'Download'),
                   column(width=4, 
                          actionButton(ns("GenerateReport"),"Generate it", style=actionButton.style)),   # "primary",
                   
                   column(width=4, 
                          downloadButton(ns("download_doc"), label="Download docx", icon=icon("download"), style=actionButton.style )),   #, icon("download")
                   
                   column(width=4,
                          downloadButton(ns("download_ppt"), label="Download pptx", icon=icon("download"), style=actionButton.style ))   # icon("paper-plane")
                )
                
             ) # box
           )
    )
  )
  
  
}

# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function
checkOut <- function(input, output, session,  DATA, FIGURE_ALL, TABLE_ALL) {
  
  ns <- session$ns
  
  #inputData = mutateData$adpc 
  
  inputData <- reactive({
    
    DATA$mDATA[[1]]  # return the first dataset
    
  })
  
  dataset2download <- reactive({
    if (is.null(input$data_name)) {return(NULL)}
    
    data = DATA$mDATA[[input$data_name]]
    data  
    
  })
  
  ########################################################### 
  ########################################################### 
  # tab: review (figure and tables)
  ########################################################### 
  ########################################################### 
  
  output$loadTemplate_container <- renderUI({
    
    fluidRow(
      column(4,
             fileInput(ns("doc_template"), label = h5("load your doc template"), 
                       accept=c('.docx'))),
      
      column(4,
             fileInput(ns("ppt_template"), label = h5("load your ppt template"), 
                       accept=c('.pptx')))   
    )
  })
  
  
  #----------------------------------------------
  # server side [Checkout]
  #----------------------------------------------
  
  output$dataset2download_Selector <- renderUI({
    
    data.name.lst =  names(DATA$mDATA)
    selectizeInput(ns("data_name"), 
                   label    = "select data to download:" , 
                   choices  = data.name.lst, 
                   multiple = FALSE,
                   selected = data.name.lst[1])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data_name, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset2download(), file= paste(input$data_name, ".csv", sep = ""), row.names = FALSE)
      
      write.csv(dataset2download(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  # output$finalFigCount <- renderText(
  #   FIGURE_ALL$NumOfFig
  # )
  # 
  # output$finalTabCount <- renderText(
  #   TABLE_ALL$NumOfTab
  # )
  
  # 
  # valueBoxOutput("finalDataCount"),
  # 
  # valueBoxOutput("finalFigCount"),
  # 
  # valueBoxOutput("finalTabCount"),
  # 
  # c("database", "bar-chart-o", "table" )
  
  output$finalDataCount <- renderValueBox({
    valueBox(
       value=length(DATA$mDATA),  subtitle="dataset(s) in total", icon = icon("database", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$finalFigCount <- renderValueBox({
    valueBox(
      value=length(FIGURE_ALL$mFIGURES), subtitle="figure(s) in total",icon = icon("bar-chart-o", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  
  output$finalTabCount <- renderValueBox({
    valueBox(
      value=length(TABLE_ALL$mTABLES),  subtitle="table(s) in total",  icon = icon("table", lib = "font-awesome"),
      color = "green"
    )
  })
  
  
  
  # load the specification file
  specInput <- reactive({
    #req(input$spec_file)
    
    spec_file <- input$spec_file
    if (is.null(spec_file)) {return(NULL)}
    
    inFile = spec_file
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    myspec <-  data.frame(read_excel(paste(inFile$datapath, ext, sep="."), sheet="PKreport", col_names = FALSE))
    
    myspec
  })
  
  # load doc template 
  docInput <- reactive({
    #req(input$doc_template)
    
     doc_template <- input$doc_template
     if (is.null(doc_template)) {return(NULL)}
    
    validate(need(input$doc_template, message = FALSE))
    
    inFile = input$doc_template   # doc_template
  
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    
    mydoc <- docx(template = paste(inFile$datapath, ext, sep="."), empty_template = FALSE)
    mydoc
    # sort(list_bookmarks(mydoc))
  })   
  
  # load ppt template
  pptInput <- reactive({
    #req(input$ppt_template)
    
     ppt_template <- input$ppt_template
     if (is.null(ppt_template)) {return(NULL)}
    
    validate(need(input$ppt_template, message = FALSE))
    
    inFile = input$ppt_template
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    
    myppt <- pptx(title = "title", template = paste(inFile$datapath, ext, sep="."))
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
    
    
    ##Insert code to build document####
    
    cat(file=stderr(), "##############Step: write2doc#################", "\n")
    
    
    # take a dependency on WriteReport action button
    #if(input$GenerateReport == 0) return()
    #if(input$download_doc == 0) return()
    
    # construct a an empty docx object   
    #doc <- pptx(template = '/insertfolderlocation/ PPT Template.pptx' )
    #docx <- docx(template = '/insertfolderlocation/Word Template.docx')
    
    #doc = docx()  
    #ppt = pptx()  
    
    # doc template
    ################################
    doc <- NULL
    doc <- docx(template = './lib/docTemplate.docx', empty_template = FALSE)
    # sort(list_bookmarks(mydoc))
    doc.bookmark.lst = list_bookmarks(doc)
    
    mydoc=NULL;  mydoc = docInput()
    if (!is.null(mydoc))  {doc=NULL; doc = mydoc} 
    
    
    # ppt template
    ################################
    data = inputData()
    my.title = paste0("Key Results for ", unique(data$STUDYID)[1])
    ppt <- NULL
    ppt<- pptx(title=my.title, template = './lib/pptTemplate.pptx')
    # slide.layouts(myppt)
     
    myppt=NULL;   myppt= pptInput()
    if (!is.null(myppt))  {ppt = NULL; ppt = myppt}    
    
    
    # in order to write text we need to specify a style
    # inspect which styles we have (note this depends on your language settings and Word version installed)
    #print(styles(doc))
    
    # inform the doc object which styles we have
    #doc = declareTitlesStyles(doc, stylenames = styles(doc))  
    
    # as the shiny app doesn't know your system a priori we simply 
    # pick a style by index e.g. 25 instead of by name
    StyleIndex <- min(9,length(styles(doc)))
    
    
    
    #insert all FlexTables to doc object
    NumOfTab <- length(TABLE_ALL$mTABLES)
    
    if (NumOfTab>0) {
      cat(file=stderr(), "##############Step: begin writing all table(s) to doc#################", "\n")
      
      for( i in seq(1, NumOfTab, by=1)) {
        
        tab.bookmark <- names(TABLE_ALL$mTABLES)[i] # [[which_fig]]
        
        # make some room by adding an empty line
        # stylename: we need to use a valid style here
        doc = addParagraph(doc,"\n", stylename = styles(doc)[1])
        
        # extract table from log
        #newTitle <- isolate(TABLE_ALL$mTITLES[[i]]) #%>% unlist())        
        newTab <- isolate(TABLE_ALL$mTABLES[[i]]) 
       # newFootnote <- isolate(TABLE_ALL$mFOOTNOTES[[i]] )#%>% unlist())        
        
        # add table to doc object
        if(tab.bookmark %in% doc.bookmark.lst) {              
          # update the body font to a smaller size
          #NewTable[,] =  textProperties(font.size = 8)       
          #addParagraph2(doc, value=newTitle, bookmark=paste0(tab.bookmark, "_title"))          
          addFlexTable(doc,  vanilla.table(newTab), bookmark=tab.bookmark)          
          #addParagraph2(doc, value=newFootnote, bookmark=paste0(tab.bookmark, "_footnote"))   
        }
        
        if(!tab.bookmark %in% doc.bookmark.lst) {
          #addParagraph2(doc, value=newTitle)           
          addFlexTable(doc,  vanilla.table(newTab))     
          #addParagraph2(doc, value=newFootnote) 
        }
        
      }
      
      cat(file=stderr(), "##############Step: finish writing all tables(s) to doc#################", "\n")
      
    }
    
     
    #insert all FlexTables to doc object
    NumOfFig <- length(FIGURE_ALL$mFIGURES)
    
    if (NumOfFig>0) {
      cat(file=stderr(), "##############Step: begin writing all figure(s) to doc#################", "\n")
      
      for( i in seq(1, NumOfFig, by=1)) {
        
        fig.bookmark <- names(FIGURE_ALL$mFIGURES)[i] # [[which_fig]]
        
        # make some room by adding an empty line
        # stylename: we need to use a valid style here
        doc = addParagraph(doc,"\n", stylename = styles(doc)[1])
        
        # extract table from log
        newTitle <- isolate(FIGURE_ALL$mTITLES[[i]]) #%>% unlist())        
        newFig <- isolate(FIGURE_ALL$mFIGURES[[i]]) 
        newFootnote <- isolate(FIGURE_ALL$mFOOTNOTES[[i]] )#%>% unlist())        
        
        fig_size <- isolate(FIGURE_ALL$mSIZE[[i]] )
        if (length(fig_size)==0) { fig_size=c(6.4, 4.8)}
         
       
        fig_width = fig_size[1]
        fig_height = fig_size[2]
         
        # add table to doc object
        if(fig.bookmark %in% doc.bookmark.lst) {
          #addParagraph2(doc, value=newTitle, bookmark=paste0(fig.bookmark, "_title"))          
          
          addPlot2docx(doc, x=newFig, fun=print, pointsize=12, bookmark=fig.bookmark, width = fig_width, height =fig_height) 
          #addPlot2docx(doc, x=newFig, fun=print, pointsize=12, bookmark=fig.bookmark, width = 6.4, height = 4.8) 
          #addParagraph2(doc, value=newFootnote, bookmark=paste0(fig.bookmark, "_footnote"))   
        }
        
        if(!fig.bookmark %in% doc.bookmark.lst) {
          #addParagraph2(doc, value=newTitle)           
          addPlot2docx(doc, x=newFig, fun=print, pointsize=12, width = fig_width, height =fig_height)
          #addParagraph2(doc, value=newFootnote) 
        }
        
        data = inputData()
        my.title = paste0("Type your title here (", unique(data$STUDYID)[1], ")")
        
        log.name = "log"
        ppt <- ppt %>%
          addSlide( slide.layout = "Title and Content" ) %>%
          addTitle(value=my.title, level=12) %>%
          addPlot2pptx( x=newFig, fun=print, pointsize=12) %>%   # function( ) print( newFig ) ) %>%
          addPageNumber() #%>%
        #addFooter(pot(fig.footnote(log.scale=log.name, LLOQ=0.078), font12_style))
        
      }
      
      cat(file=stderr(), "##############Step: finish writing all figure(s) to doc#################", "\n")
      
    }
    
    
    # construct a word document based on the constructed doc object
    #writeDoc(doc,"Report2.docx")
    
    # open result
    #browseURL("Report2.docx")
    
    
    ###Create list of reactive object
    D$documents <- list(doc, ppt)
  })
  
  
  
  
  
  
  ###################ReporteRs Download Handler Word####################################################################
  output$download_doc2 <- downloadHandler(
    filename = "Network Analysis.docx",
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      doc <- D$documents[[1]]
      validate(
        need(!is.null(doc), "You have to press Build Network Content First and let it run, usually takes 1-2 minutes.")
      )
      writeDoc(doc,file)
    })
  
  
  
  output$download_doc <- downloadHandler(
    
    filename = function() { 
      data = inputData()
      studyid = unique(as.character(data$STUDYID))[1]
      paste(studyid,"-PK-report.docx", sep="")
    },
    
    content = function(file) {
      if (is.null(inputData()))  {return(NULL)   }
      
      tmpdir <- setwd(tempdir())
      on.exit(setwd(tmpdir))
      
      
      cat(file=stderr(), "##############Step: download docx #################", "\n")
      
      #doc = write2doc()   
      
      #writeDoc(write2doc(), file = paste(studyid,"-PK-report1.docx", sep=""))
      #writeDoc(write2doc(), file = paste(studyid,"-PK-report2.docx", sep=""))
      
      #writeDoc(doc$myppt, file = paste(studyid,"-PK-pptx.pptx", sep=""))
      
      doc <- D$documents[[1]]
      validate(
        need(!is.null(doc), "You have to press 'Generate it' First and let it run, usually takes a few seconds.")
      )
      
      
      writeDoc(doc,file=file)
      
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
      data = inputData()
      studyid = unique(as.character(data$STUDYID))[1]
      paste(studyid,"-PK-report.pptx", sep="")
      
    },
    
    content = function(file) {
      if (is.null(inputData()))  {return(NULL)   }
      
      tmpdir <- setwd(tempdir())
      on.exit(setwd(tmpdir))
      
      
      cat(file=stderr(), "##############Step: download pptx #################", "\n")
      
      #doc = write2doc()   
      
      #writeDoc(write2doc(), file = paste(studyid,"-PK-report1.docx", sep=""))
      #writeDoc(write2doc(), file = paste(studyid,"-PK-report2.docx", sep=""))
      
      #writeDoc(doc$myppt, file = paste(studyid,"-PK-pptx.pptx", sep=""))
      
      doc <- D$documents[[2]]
      validate(
        need(!is.null(doc), "You have to press 'Generate It' First and let it run, usually takes a few seconds.")
      )
      
      #writeDoc(doc, file="./doc/tmp.pptx")
      writeDoc(doc,file=file)
      
      # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
      #zip(zipfile=file, files=c(fileDOC, filePPT) )
      #browseURL(fileDOC)
      
    }
    #contentType = "application/zip"
    #contentType="application/octet-stream"  #ms-word
    #contentType="application/ms-word"  #ms-word
  ) 
  
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("figure review accordingly.")
    cat(msg, "\n")
  })
  
  
  # Return the reactive that yields the data frame
  #return(inputData)
  # return all input as a list?
}
