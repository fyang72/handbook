

print2ppt <- function(myppt, FIGURE, TABLE) { 
  ########################################################################################
  ########################################################################################
  # to word and ppt
  ########################################################################################
  ########################################################################################
  library(ReporteRs)

  default_pptx_width=8;     # 8
  default_pptx_height=5.2;    # 6
  
  fontsize_default=12; 
  title_default = "Type in title"
  
  
  if (is.null(myppt)) {myppt <- pptx() } #title = "title", template = './docs/pptTemplate.pptx')}

   
  
  #print(styles(myppt))
  
  # inform the doc object which styles we have
  #doc = declareTitlesStyles(doc, stylenames = styles(doc))  
  
  # as the shiny app doesn't know your system a priori we simply 
  # pick a style by index e.g. 25 instead of by name
  #StyleIndex <- min(9,length(styles(myppt)))
  
  
  
  #--------------------------------------------------
  #insert all FlexTables to doc object
  #--------------------------------------------------
  
  tabl.name.lst <- names(TABLE) 
  NumOfTab <- length(tabl.name.lst)
  
  if (NumOfTab>0) {
    cat(file=stderr(), "##############Step: begin writing all table(s)#################", "\n")
    
    for( i in seq(1, NumOfTab, by=1)) {
      tabl.name = tabl.name.lst[i]
      print(tabl.name) 
   
      # extract table from log
      newTab <- TABLE[[tabl.name]] 
      title = ifelse(is.null(attr(newTab, "title")), "No title yet", attr(newTab, "title")) 
      footnote = ifelse(is.null(attr(newTab, "footnote")), "", attr(newTab, "footnote"))  
         
    }
  }
  
  
  #--------------------------------------------------
  # add figures
  #--------------------------------------------------
  fig.name.lst = names(FIGURE)
  #fig.name.lst <- intersect(fig.name.lst, doc.bookmark.lst)
  NumOfFig <- length(FIGURE)
  
  if (NumOfFig>0) {
    cat(file=stderr(), "##############Step: begin writing all figure(s) to doc#################", "\n")
    
    for( i in seq(1, NumOfFig, by=1)) {
      
      fig.name = fig.name.lst[i]
      print(fig.name)
      
      # make some room by adding an empty line   stylename: we need to use a valid style here
      #doc = addParagraph(doc,"\n", stylename = styles(doc)[1])
      
      newFig <- FIGURE[[fig.name]]
      title = attr(newFig, "title"); title = ifelse(is.null(title), "No title yet", title) 
      footnote = attr(newFig, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
      
     pptx_width =  attr(newFig, "pptx_width"); pptx_width = ifelse(is.null(pptx_width), default_pptx_width, pptx_width)
      pptx_height =  attr(newFig, "pptx_height"); pptx_height = ifelse(is.null(pptx_height), default_pptx_height, pptx_height)
      
      fontsize =  attr(newFig, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
       
      
      myppt <- myppt %>%
        addSlide( slide.layout = "Title and Content" ) %>%
        addTitle(value=title, level=12) %>%
        addPlot2pptx( x=newFig, fun=print, pointsize=fontsize, width=pptx_width, height=pptx_height) %>%   # function( ) print( newFig ) ) %>%
        addPageNumber() #%>%
      #addFooter(pot(fig.footnote(log.s
      
      
      
    } }
  
  return(myppt)
  
}




 
addPlot2pptx <- function(myppt, x,   fun=print, pointsize=12, width = 8, height = 5.2) {
  
  ## S3 method for class 'pptx'
  #addPlot(doc, fun, pointsize = 11, vector.graphic = TRUE,
  #  fontname = getOption("ReporteRs-default-font"),
  #  fontname_serif = "Times New Roman", fontname_sans = "Calibri",
  #  fontname_mono = "Courier New", fontname_symbol = "Symbol",
  #  editable = TRUE, offx, offy, width, height, bg = "transparent", ...)
  #   
  
  offx = 2.4
  offy = 2.0
  
  if(width==8 & height==5.23643476) { 
    offx = 2.4
    offy = 2.0
  } 
  
  addPlot(myppt, x=x, fun=print, pointsize=pointsize, vector.graphic = FALSE, width = width, height =height    
          , fontname_serif = "Times New Roman", fontname_sans = "Calibri" 
          , offx =offx, offy=offy 
          , par.properties = parProperties(text.align = "center", padding = 5)  # not useful
  )
  
}

