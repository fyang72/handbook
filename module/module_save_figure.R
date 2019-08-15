
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
    
    fluidRow(  #column(width=12, div(align = "center",
      
      column(width=2, offset = 1, #status = "primary",  #class = 'rightAlign',#background ="aqua",
             actionButton(ns("figure_options"),label="Options", style=actionButton_style)
      ), 
      
      column(width=3, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             uiOutput(ns("figure_name_container"))
             
      ),
       
      column(width=2, #status = "primary",  #class = 'rightAlign',#background ="aqua",
             actionButton(ns("figure_saveit"),label="Save it", style=actionButton_style)
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloaddoc"),label="docx", icon=icon("download"), style=actionButton_style)                                                                        
      ), 
      
      column(width=2,                                                                                                                                                                   
             downloadButton(ns("downloadppt"),label="pptx", icon=icon("download"), style=actionButton_style)                                                                        
      )
    ), #)), 
    
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
values <- reactiveValues()

# figure_title_container
output$figure_title_container <- renderUI(renderText({
  figure <- ggplot_figure()
  paste0("<b>Figure ", figure_index, ": ", attributes(figure)$title, "</b>")
})() %>% HTML()
)

# figure_name_container
output$figure_name_container <- renderUI({
  validate(need(figure, message="no figure found")
  )
  
textInput(ns("figure_name"), value=figure_name, label=NULL)
})
 
ggplot_figure <- reactive({
   
  # title
  if (!is.null(values$title)) {   
    attr(figure, "title") = values$title
  }
  
  
  if (!is.null(values$xlabel)) {  
  figure  <- figure  + labs(x = values$xlabel)
  }
  
  #validate(values$plot_y_label, message=FALSE)
  if (!is.null(values$ylabel)) { 
  figure  <- figure  + labs(y = values$ylabel)
  }
  
  #
  # width and height (pptx)
  if (!is.null(values$pptx_width)) {   
    attr(figure, "pptx_width") = as.numeric(values$pptx_width)
  }
  if (!is.null(values$pptx_height)) {   
    attr(figure, "pptx_height") = as.numeric(values$pptx_height)
  }
  
  # width and height (docx)
  if (!is.null(values$docx_width)) {   
    attr(figure, "docx_width") = as.numeric(values$docx_width)
  }
  if (!is.null(values$docx_height)) {   
    attr(figure, "docx_height") = as.numeric(values$docx_height)
  }  
  
  # fontsize
  if (!is.null(values$fontsize)) {   
    #attr(figure, "fontsize") = as.numeric(values$fontsize)
    figure <- figure + theme_regn(font_size = as.numeric(values$fontsize))
  }
  
  # hline
  if (!is.null(values$hline)) {   
    #attr(figure, "fontsize") = as.numeric(values$fontsize)
    
    hline <- eval(parse(text=paste0("c(", values$hline, ")")))
    hline_location = eval(parse(text=paste0("c(", values$hline_location, ")")))
    #hline_label <- eval(parse(text=paste0("c(", values$hline_label, ")")))
    
    hline <- str_split(values$hline, ", ") %>% unlist() %>% as_numeric()  
    hline_location = str_split(values$hline_location, ", ") %>% unlist() %>% as_numeric()   #eval(parse(text=paste0("c(", values$hline_location, ")")))
    hline_label <- str_split(values$hline_label, ", ") %>% unlist() %>% as.character()
    
    figure <- figure + 
      geom_hline(yintercept = hline, lty="dashed") + 
      annotate("text", hline_location, hline,
               vjust = -1,  label =  hline_label  
               )   
  }
  
  # # vline
  # if (!is.null(values$vline)) {   
  #   #attr(figure, "fontsize") = as.numeric(values$fontsize)
  #   figure <- figure + geom_vline(xintercept = values$vline, lty="dashed") + 
  #     annotate("text", values$vline_location, values$vline, vjust = -1, label = values$vline_label)   
  # }
  
   
  figure
})




#----------------------------------------------------
# figure_container
#----------------------------------------------------
output$figure_container <-renderUI({
  validate(need(ggplot_figure(), message="no figure found")
  )
  
  #call module  
  callModule(module_ggplot_brush, "module_ggplot_brush_for_save_figure", 
                   fig=ggplot_figure(), mydata=figure_data, xvar="xvar", yvar="yvar"
             )
 
  # UI  
  fluidRow(column(12, 
                  module_ggplot_brush_UI(ns("module_ggplot_brush_for_save_figure"), label=NULL)
                  ) 
          )  
})  



#   
output$figure_container2 <- renderUI({ 
 validate(need(figure, message="no figure found")
 ) 
  
  library(plotly)
  # renderPlotly() also understands ggplot2 objects!
  output$figure2 <- renderPlotly({
    figure %>% plotly::ggplotly()
  })
 
  plotlyOutput(ns("figure2"), width = "100%", height = "500px")
 })


#-------------------------------------------------------------
# save figure 
#-------------------------------------------------------------
#add figure to log when action button is pressed
observeEvent(input$figure_saveit, {

validate(need(input$figure_name, message=FALSE),
         need(figure, message="no figure found")
)

  attr(figure, "data_name") = "data_name"
  ALL$FIGURE[[input$figure_name]]  = figure
  
  showNotification("Save figure sucessfully", type="message")   # "default, "message", "warning", "error"
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
    paste0(input$figure_name, ".pptx")    
  },
  
  content = function(file) {
    #if (is.null(inputData()))  {return(NULL)   }
    
    #tmpdir <- setwd(tempdir())
    #on.exit(setwd(tmpdir))
      
    #tt <- print2_word_ppt2(myfig, TABLE_ALL=NULL, mydoc=NULL, myppt=NULL)
    mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate_long_format.pptx")) %>%  
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
 

observeEvent(input$figure_options, {
  showModal(plotModel())
})


plotModel <- function(){
  my_plot <- ggplot_figure()  #req(zoomed_plot())$plot
   
  modalDialog(
    fluidRow(column(12, textInput(ns("title"), "title", width = "100%", value = attr(my_plot, "title")))),
    #fluidRow(column(12, textInput(ns("subtitle"), "sub-title", width = "100%", value =  my_plot$labels$subtitle))),
    
    fluidRow(column(6, textInput(ns("xlabel"), "x-axis label", width = "100%", value = my_plot$labels$x)),
             column(6, textInput(ns("ylabel"), "y-axis label", width = "100%", value = my_plot$labels$y))),
    
    fluidRow(column(3, textInput(ns("hline"), "hline(s)", width = "100%", value = NULL, placeholder="0.078, 10" )),
             column(3, textInput(ns("hline_location"), "location(s)", width = "100%", value = NULL, placeholder="0, 0" )), 
             column(6, textInput(ns("hline_label"), "label(s)", width = "100%", value = NULL, placeholder="0.078 mg/L, 10 mg/L" ))
             
             # column(2, textInput(ns("vline"), "vline(s)", width = "100%", value = NULL, placeholder="28, 56" )),
             # column(2, textInput(ns("vline_label"), "vlabel(s)", width = "100%", value = NULL, placeholder="Day 28, Day 56" )),
             # column(2, textInput(ns("vline_location"), "hlocation(s)", width = "100%", value = NULL, placeholder="10, 10" ))
             ),
    
    fluidRow(
      column(3, numericInput(ns("docx_width"), "width(docx)",
        width = "100%", 
        value = ifelse(is.null(attr(my_plot, "docx_width")), 6.4, attr(my_plot, "docx_width")))),
             
      column(3, numericInput(ns("docx_height"), "height(docx)", 
        width = "100%", 
        value = ifelse(is.null(attr(my_plot, "docx_height")), 4.8, attr(my_plot, "docx_height")))),
      
      column(3, numericInput(ns("pptx_width"), "width(pptx)",
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "pptx_width")), 8, attr(my_plot, "pptx_width")))),
      
      column(3, numericInput(ns("pptx_height"), "height(pptx)", 
                             width = "100%", 
                             value = ifelse(is.null(attr(my_plot, "pptx_height")), 5.2, attr(my_plot, "pptx_height"))))      
      ),
    
    fluidRow(
      column(3, numericInput(
        ns("fontsize"), "fontsize", 
        width = "100%", 
        value = ifelse(is.null(attr(my_plot, "fontsize")), 12, attr(my_plot, "fontsize"))))), 
    
    #fluidRow(column(3, selectInput(ns("theme"), "Theme", choices = c("Default", "Grey", "White", "Minimal"), selected = "Default"))),
    #fluidRow(column(6,
    #                numericInput(ns("axis_label_rotation"), "Axis labels rotation (°)", value = 0))),
    
    br(),
    fluidRow(column(12, actionButton(ns("save"), "save", style=actionButton_style))),
    footer = NULL,
    easyClose = TRUE
  )
}

observeEvent(input$save, {
  values$title<- input$title
  values$subtitle<- input$subtitle
  
  
  values$xlabel<- input$xlabel
  values$ylabel<- input$ylabel
  
  values$docx_width <- input$docx_width
  values$docx_height<- input$docx_height
  
  values$pptx_width <- input$pptx_width
  values$pptx_height<- input$pptx_height
  
  values$hline<- input$hline
  #values$vline<- input$vline
  values$hline_label <- input$hline_label
  #values$vline_label <- input$vline_label
  values$hline_location <- input$hline_location
  #values$vline_location <- input$vline_location  
  
  values$fontsize<- input$fontsize
  
})

 
# We can run observers in here if we want to
# observe({
#   msg <- sprintf("figure review accordingly.")
#   cat(msg, "\n")
# })

return(ALL)
}

