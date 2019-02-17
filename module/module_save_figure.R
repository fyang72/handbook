




print2_word_ppt <- function(FIGURE_ALL, TABLE_ALL,   
                            mydoc=NULL, myppt=NULL, 
                            width_default=6.4,     # 8
                            height_default=4.8,    # 6
                            fontsize_default=12, 
                            title_default = "Type in title"
) { 
  ########################################################################################
  ########################################################################################
  # to word and ppt
  ########################################################################################
  ########################################################################################
  library(ReporteRs)
  
  if (is.null(myppt)) {myppt <- pptx(title = "title", template = './lib/pptTemplate.pptx')}
  if (is.null(mydoc)) {mydoc <- docx(template = './lib/docTemplate.docx', empty_template = FALSE)}
  
  
  # sort(list_bookmarks(mydoc))
  doc.bookmark.lst = list_bookmarks(mydoc)
  
  #print(styles(mydoc))
  
  # inform the doc object which styles we have
  #doc = declareTitlesStyles(doc, stylenames = styles(doc))  
  
  # as the shiny app doesn't know your system a priori we simply 
  # pick a style by index e.g. 25 instead of by name
  StyleIndex <- min(9,length(styles(mydoc)))
  
  
  
  #--------------------------------------------------
  #insert all FlexTables to doc object
  #--------------------------------------------------
  
  tabl.name.lst <- names(TABLE_ALL)
  #tabl.name.lst <- intersect(tabl.name.lst, doc.bookmark.lst)
  NumOfTab <- length(tabl.name.lst)
  
  if (NumOfTab>0) {
    cat(file=stderr(), "##############Step: begin writing all table(s) to doc#################", "\n")
    
    for( i in seq(1, NumOfTab, by=1)) {
      
      tabl.name = tabl.name.lst[i]
      print(tabl.name) 
      
      # make some room by adding an empty line stylename: we need to use a valid style here
      doc = addParagraph(mydoc,"\n", stylename = styles(mydoc)[1])
      
      # extract table from log
      newTab <- TABLE_ALL[[tabl.name]] 
      title = ifelse(is.null(attr(newTab, "title")), "No title yet", attr(newTab, "title")) 
      footnote = ifelse(is.null(attr(newTab, "footnote")), "", attr(newTab, "footnote"))  
      
      #
      if (paste0(tabl.name, "_title") %in% doc.bookmark.lst)  addParagraph2(mydoc, value=title, bookmark=paste0(tabl.name, "_title")) 
      if (tabl.name %in% doc.bookmark.lst)  addFlexTable(mydoc, vanilla.table(newTab), bookmark=tabl.name)  
      if (paste0(tabl.name, "_footnote") %in% doc.bookmark.lst)   addParagraph2(mydoc, value=footnote, bookmark=paste0(tabl.name, "_footnote"))    
      
      if (!tabl.name %in% doc.bookmark.lst) {
        addParagraph2(mydoc, value=title)
        addFlexTable(mydoc,  vanilla.table(newTab) )  
        addParagraph2(mydoc, value=footnote)
      }
      
      
      myppt <- myppt %>%
        addSlide( slide.layout = "Title and Content" ) %>%
        addTitle(value=title, level=12) %>%
        #addPlot2pptx( x=newFig, fun=print, pointsize=fontsize, width=width, height=height) %>%   # function( ) print( newFig ) ) %>%
        addFlexTable(vanilla.table(newTab)) %>% 
        addPageNumber() #%>%
      #addFooter(pot(fig.footnote(log.s
    
      }}
  
  
  #--------------------------------------------------
  # add figures
  #--------------------------------------------------
  fig.name.lst = names(FIGURE_ALL)
  #fig.name.lst <- intersect(fig.name.lst, doc.bookmark.lst)
  NumOfFig <- length(FIGURE_ALL)
  
   
  
  if (NumOfFig>0) {
    cat(file=stderr(), "##############Step: begin writing all figure(s) to doc#################", "\n")
    
    for( i in seq(1, NumOfFig, by=1)) {
      
      fig.name = fig.name.lst[i]
      print(fig.name)
      
      # make some room by adding an empty line   stylename: we need to use a valid style here
      #doc = addParagraph(doc,"\n", stylename = styles(doc)[1])
      
      print("NumOfFig inside")
      
      
      newFig <- FIGURE_ALL[[fig.name]]
      title = attr(newFig, "title"); title = ifelse(is.null(title), "No title yet", title) 
      footnote = attr(newFig, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
      width =  attr(newFig, "width"); width = ifelse(is.null(width), width_default, width)
      height =  attr(newFig, "height"); height = ifelse(is.null(height), height_default, height)
      fontsize =  attr(newFig, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
      
      if (paste0(fig.name, "_title") %in% doc.bookmark.lst)  addParagraph2(mydoc, value=title, bookmark=paste0(fig.name, "_title")) 
      if (fig.name %in% doc.bookmark.lst)  addPlot2docx(mydoc, x=newFig, fun=print, pointsize=fontsize, bookmark=fig.name, width=width, height=height) 
      if (paste0(fig.name, "_footnote") %in% doc.bookmark.lst)   addParagraph2(mydoc, value=footnote, bookmark=paste0(fig.name, "_footnote"))    
      
      if (!fig.name %in% doc.bookmark.lst) {
        addParagraph2(mydoc, value=title)
        addPlot2docx(mydoc, x=newFig, fun=print, pointsize=fontsize, width=width, height=height)  
        addParagraph2(mydoc, value=footnote)
      }
      
      
      myppt <- myppt %>%
        addSlide( slide.layout = "Title and Content" ) %>%
        addTitle(value=title, level=12) %>%
        addPlot2pptx( x=newFig, fun=print, pointsize=fontsize, width=width, height=height) %>%   # function( ) print( newFig ) ) %>%
        addPageNumber() #%>%
      #addFooter(pot(fig.footnote(log.s
      
      
      
    } }
  
  return(list(mydoc=mydoc, myppt=myppt) )
  
}






addPlot2docx <- function(mydoc, x, bookmark=NULL, fun=print, pointsize=12, width=6.4, height=4.8) {
  ## S3 method for class 'docx'
  #addPlot(doc, fun, pointsize = getOption("ReporteRs-fontsize"),
  #  vector.graphic = FALSE, width = 6, height = 6,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, bookmark, par.properties = parProperties(text.align =
  #  "center", padding = 5), bg = "transparent", ...)
  # 
  
  ## S3 method for class 'pptx'
  #addPlot(doc, fun, pointsize = 11, vector.graphic = TRUE,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, offx, offy, width, height, bg = "transparent", ...)
  #   
  addPlot(mydoc, x=x, fun=print, pointsize=12, bookmark=bookmark, vector.graphic = FALSE, width = width, height = height
          , fontname_serif = "Times New Roman", fontname_sans = "Calibri"  # "Calibri" 
          , par.properties = parProperties(text.align = "center", padding = 5)
  )
}



addPlot2pptx <- function(mydoc, x,   fun=print, pointsize=12, width = 6.4, height = 4.8) {
  
  ## S3 method for class 'pptx'
  #addPlot(doc, fun, pointsize = 11, vector.graphic = TRUE,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, offx, offy, width, height, bg = "transparent", ...)
  #   
  
  offx = ifelse(width==6.4 & height==4.8, 1.8, 
                ifelse(width==8 & height==6, 1.15, 1.8))
  
  offy = ifelse(width==6.4 & height==4.8, 1.8, 
                ifelse(width==8 & height==6, 1.35, 1.8))
  
  addPlot(mydoc, x=x, fun=print, pointsize=12, vector.graphic = FALSE, width = width, height =height    
          , fontname_serif = "Times New Roman", fontname_sans = "Calibri" 
          , offx =offx, offy=offy 
          , par.properties = parProperties(text.align = "center", padding = 5)  # not useful
  )
  
}





################################################################################ 
# module_load_cppModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_save_figure_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
   
  validate(need(globalVars$login$status, message=FALSE)
  )
   
  fluidRow(  
    
    fluidRow(
      column(12, uiOutput(ns("figure_title_container")))
    ), 
    
    fluidRow(
      column(width=12, 
             #plotOutput(ns("fig"), width = "100%", height = "500px")
             uiOutput(ns("figure_container"))
      )
    ), 
    
    fluidRow(
      column(width=6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             uiOutput(ns("figure_name_container"))
             
      ),
      
      column(width=2, #status = "primary",  #class = 'rightAlign',#background ="aqua",
             actionButton(ns("figure_saveit"),label="Save it", style=actionButton.style)
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloaddoc"),label="doc", icon=icon("download"), style=actionButton.style)                                                                        
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloadppt"),label="ppt", icon=icon("download"), style=actionButton.style)                                                                        
      )
    ), 
    
    style='margin-bottom:30px;  border:1px solid; padding: 10px;'
  )
  
} 


################################################################################ 
# main function: module_load_cppModel
################################################################################

module_save_figure <- function(input, output, session, ALL, 
                               figure=NULL, 
                               figure_index = 1, 
                               figure_name="fig-", 
                               figure_data =NULL
                               ){
  
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  
  output$figure_title_container <- renderUI(renderText({
    paste0("Figure ", figure_index, " ", attributes(figure)$title)
  })() %>% HTML()
  
  )
  
  output$figure_name_container <- renderUI({
    validate(need(figure, message="no figure found")
    )
    
  textInput(ns("figure_name"), value=figure_name, label=NULL)
  
  })
   
  #----------------------------------------------------
  # figure_container
  #----------------------------------------------------
  output$figure_container <-renderUI({
    validate(need(figure, message="no figure found")
    )
    
    #print("figure found at line 287 in save figure")
    # callModule 
    #isolate({ 
    callModule(module_ggplot_brush, "module_ggplot_brush_for_save_figure", 
                     fig=figure, mydata=figure_data, xvar="xvar", yvar="yvar")
    #})
    
    # UI  
    fluidRow(column(12, 
                    module_ggplot_brush_UI(ns("module_ggplot_brush_for_save_figure"), label=NULL)
                   ) 
             )  
  })  
  
#   
#   output$figure_container <- renderUI({
#     #validate(need(figure, message="no figure found"))
#     validate(need(figure, message="no figure found")
#     )
#     
#     output$figure2 <- renderPlot({  
#       
#       cat(file=stderr(), "##############Step: render myPlot #################", "\n")
#       isolate(figure)
#     })
#     
#     plotOutput(ns("figure2"), width = "100%", height = "500px")
# })


#-------------------------------------------------------------
# save figure 
#-------------------------------------------------------------
#add figure to log when action button is pressed
observeEvent(input$figure_saveit, {
  
  validate(need(input$figure_name, message=FALSE),
           need(figure, message="no figure found")
  )
  
  #cat(file=stderr(), "##############Save to ALL$FIGURE #################", "\n")
  #print(paste0("figure_name=", input$figure_name))
    
    attr(figure, "data_name") = "data_name"
    #ALL$FIGURE[[input$figure_name]] <- ggplot(data=data.frame(x=1,y=1), aes(x, y)) + geom_point()
    #ALL$FIGURE  <- figure  #ggplot(data=data.frame(x=1,y=1), aes(x, y)) + geom_point()
     
    print("Save figure sucessfully")
    ALL$FIGURE[[input$figure_name]]  = figure
    
})

  
  
  output$downloaddoc <- downloadHandler(
    filename = function() {     
      paste0(input$figure_name, ".docx")                                                                                                                                                                       
    },
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      #mydoc <-docx()    # D$documents[[1]]
      
      #myfig <- ALL$FIGURE[input$figure_name]
      myfig <- NULL
      myfig[[input$figure_name]] <- figure  # ALL$FIGURE[[input$figure_name]]
      
      tt <- print2_word_ppt(myfig, TABLE_ALL=NULL,   
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
      paste0(input$figure_name, ".pptx")    
    },
    
    content = function(file) {
      #if (is.null(inputData()))  {return(NULL)   }
      
      #tmpdir <- setwd(tempdir())
      #on.exit(setwd(tmpdir))
      
      #myppt <-pptx()    
      myfig <- NULL
      myfig[[input$figure_name]] <- figure
      
      tt <- print2_word_ppt(myfig, TABLE_ALL=NULL,   
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
   
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("figure review accordingly.")
    cat(msg, "\n")
  })

  
return(ALL)
  
}

