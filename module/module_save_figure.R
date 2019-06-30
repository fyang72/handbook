
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

# figure_title_container
output$figure_title_container <- renderUI(renderText({
  paste0("<b>Figure ", figure_index, ": ", attributes(figure)$title, "</b>")
})() %>% HTML()
)

# figure_name_container
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
  
  #call module  
  callModule(module_ggplot_brush, "module_ggplot_brush_for_save_figure", 
                   fig=figure, mydata=figure_data, xvar="xvar", yvar="yvar"
             )
 
  # UI  
  fluidRow(column(12, 
                  module_ggplot_brush_UI(ns("module_ggplot_brush_for_save_figure"), label=NULL)
                  ) 
          )  
})  

#   
#   output$figure_container <- renderUI({ 
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
# observe({
#   msg <- sprintf("figure review accordingly.")
#   cat(msg, "\n")
# })

return(ALL)
}

