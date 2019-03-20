



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
  
  if (is.null(myppt)) {myppt <- pptx() } #title = "title", template = './docs/pptTemplate.pptx')}
  if (is.null(mydoc)) {mydoc <- docx() } # template = './docs/memoTemplate.docx', empty_template = FALSE)}
  
  
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
      }}}
  
  
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
  
  offx = ifelse(width==6.4 & height==4.8, 3.5, 
             ifelse(width==8 & height==6, 2.15, 3.5))
  
  offy = ifelse(width==6.4 & height==4.8, 1.8, 
                ifelse(width==8 & height==6, 1.35, 1.8))
  
  addPlot(mydoc, x=x, fun=print, pointsize=12, vector.graphic = FALSE, width = width, height =height    
          , fontname_serif = "Times New Roman", fontname_sans = "Calibri" 
          , offx =offx, offy=offy 
          , par.properties = parProperties(text.align = "center", padding = 5)  # not useful
  )
  
}

