 


#-------------------------------------------------------------
# save figure 
#-------------------------------------------------------------
#add figure to log when action button is pressed
observeEvent(input[[paste0("figure_saveit", i)]] {
  
  validate(need(input[[paste0("figure_name", i)]], message=FALSE),
           need(values$figures[[i]], message="no figure found")
  )
  
  attr(values$figures[[i]], "data_name") = "data_name"
  ALL$FIGURE[[input[[paste0("figure_name", i)]]]]  = values$figures[[i]]
  
  showNotification("Save figure sucessfully", type="message")   # "default, "message", "warning", "error"
})



output$downloaddoc <- downloadHandler(
  filename = function() {     
    paste0(input[[paste0("figure_name", i)]], ".docx")                                                                                                                                                                       
  },
  
  content = function(file) {
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    #owd <- setwd(tempdir())
    #on.exit(setwd(owd))
    #mydoc <-docx()    # D$documents[[1]]
    
    #myfig <- NULL
    #myfig[[input$figure_name]] <-   
    
    #tt <- print2_word_ppt2(myfig, TABLE_ALL=NULL, mydoc=NULL, myppt=NULL)
    mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx")) %>% 
      print2docx(FIGURE=list("myfig"=ggplot_figure()))
    validate(need(!is.null(mydocx), "no docx found"))
    
    #writeDoc(mydocx,file)
    print(mydocx, target = file)
  })


output$downloadppt <- downloadHandler(
  
  filename = function() { 
    paste0(input[[paste0("figure_name", i)]], ".pptx")    
  },
  
  content = function(file) {
    #if (is.null(inputData()))  {return(NULL)   }
    
    #tmpdir <- setwd(tempdir())
    #on.exit(setwd(tmpdir))
    
    #tt <- print2_word_ppt2(myfig, TABLE_ALL=NULL, mydoc=NULL, myppt=NULL)
    mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate.pptx")) %>%  
      print2pptx(FIGURE=list("myfig"=ggplot_figure()))
    validate(need(!is.null(mypptx), "no pptx found"))
    
    #writeDoc(mypptx, file=file)
    print(mypptx, target = file)
    
    # http://stackoverflow.com/questions/40314582/how-to-download-multiple-reports-created-using-r-markdown-and-r-shiny-in-a-zip-f/40324750#40324750
    #zip(zipfile=file, files=c(fileDOC, filePPT) )
    #browseURL(fileDOC)
  }
  #contentType = "application/zip"
  #contentType="application/octet-stream"  #ms-word
  #contentType="application/ms-word"  #ms-word
) 


observeEvent(input[[paste0("figure_options",i)]], {
  showModal(plotModel(values$figures, i))
})


plotModel <- function(figures, i){
  my_plot <- figures[[i]]  #ggplot_figure()  #req(zoomed_plot())$plot
  
  modalDialog(
    fluidRow(
      # column(6,textInput(
      #   ns("title"), "title", 
      #   width = "100%", 
      #   value = attr(my_plot, "title"))), 
      column(3, textInput(ns(paste0("xlabel", i)), "x-axis label", width = "100%", value = my_plot$labels$x)),
      column(3, textInput(ns(paste0("ylabel", i)), "y-axis label", width = "100%", value = my_plot$labels$y)),
      
      column(4, selectizeInput(
        ns(paste0("facet_by", i)), "facet_by", 
        choices = c("", colnames(my_plot$data)), 
        selected = NULL)
      ),
      column(2, numericInput(
        ns(paste0("fontsize", i)), "fontsize", 
        width = "100%", 
        value = ifelse(is.null(attr(my_plot, "fontsize")), 12, attr(my_plot, "fontsize")))
      )
    ),
    
    fluidRow(
      column(3, radioButtons(  
        ns(paste0("xscale", i)), "x-axis scale", 
        inline = TRUE,
        choices = c("nonlog", "log"),  
        selected = "nonlog")
      ), 
      column(3, radioButtons(  
        ns(paste0("yscale", i)), "y-axis scale", 
        inline = TRUE,
        choices = c("nonlog", "log"),  
        selected = "nonlog")
      )
    ),
    
    fluidRow(column(3, textInput(ns(paste0("hline", i)), "hline(s)", width = "100%", value = NULL, placeholder="0.078, 10" )),
             column(3, textInput(ns(paste0("hline_location", i)), "location(s)", width = "100%", value = NULL, placeholder="0, 0" )), 
             column(6, textInput(ns(paste0("hline_label", i)), "label(s)", width = "100%", value = NULL, placeholder="0.078 mg/L, 10 mg/L" ))
             
             # column(2, textInput(ns("vline"), "vline(s)", width = "100%", value = NULL, placeholder="28, 56" )),
             # column(2, textInput(ns("vline_label"), "vlabel(s)", width = "100%", value = NULL, placeholder="Day 28, Day 56" )),
             # column(2, textInput(ns("vline_location"), "hlocation(s)", width = "100%", value = NULL, placeholder="10, 10" ))
    ),
    
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(3, numericInput(ns(paste0("docx_width", i)), "width(docx)",
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "docx_width")), 6.4, attr(my_plot, "docx_width")))),
      
      column(3, numericInput(ns(paste0("docx_height", i)), "height(docx)", 
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "docx_height")), 4.8, attr(my_plot, "docx_height")))),
      
      column(3, numericInput(ns(paste0("pptx_width", i)), "width(pptx)",
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "pptx_width")), 8, attr(my_plot, "pptx_width")))),
      
      column(3, numericInput(ns(paste0("pptx_height", i)), "height(pptx)", 
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "pptx_height")), 5.2, attr(my_plot, "pptx_height"))))      
    ),
    
    
    
    #fluidRow(column(3, selectInput(ns("theme"), "Theme", choices = c("Default", "Grey", "White", "Minimal"), selected = "Default"))),
    #fluidRow(column(6,
    #                numericInput(ns("axis_label_rotation"), "Axis labels rotation (°)", value = 0))),
    
    br(),
    fluidRow(column(12, actionButton(ns(paste0("save", i)), "save", style=actionButton_style))),
    footer = NULL,
    easyClose = TRUE
  )
}

observeEvent(input[[paste0("save", i)]], {
  
  
  ns_lst <- c("title", "subtitle", 
              "xlabel", "ylabel", "xscale", "yscale", 
              "docx_width",  "docx_height", "pptx_width", "pptx_height", 
              "hline", "hline_label", "hline_location", "fontsize", "facet_by"
  )
          
  ns_lst <- lapply(1:length(ns_lst), function(ins) {
    paste0(ns_lst[ins], i)
  })  %>% unlist()
  
  values[ns_lst] = input[ns_lst]
   
})
 

 
