
#http://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

module_ggplot_brush_UI <- function(id, label="") {
  
  ns <- NS(id)
  tagList(
     #h4("NTIM vs TIME"),
     uiOutput(ns("ggplot_container")),

     # h4("Selected Data"),
     uiOutput(ns("info_container"))
  )
}


module_ggplot_brush_UI2 <- function(id, label="") {
  
  ns <- NS(id)
   
  fluidRow(
    column(width = 6,
           #h4("NTIM vs TIME"),
           uiOutput(ns("ggplot_container"))
    ),
    column(width = 6,
           fluidRow(
             h4("Selected Data"),
             uiOutput(ns("info_container"))
           )
    )
  )
}


################################################################################ 
# log in and load data (sidebar)
################################################################################

module_ggplot_brush <- function(input, output, session, fig, 
                                mydata, xvar="xvar", yvar="yvar") {
  
  ns <- session$ns

  ranges <- reactiveValues(x = NULL, y = NULL)
  
  mydata = fig$data 
  
  xvar=NULL
  yvar=NULL
  if(!is.null(fig$mapping)) { 
    xvar = rlang::get_expr(fig$mapping[[1]])%>% as.character()
    if (length(fig$mapping)==2) {
      yvar = rlang::get_expr(fig$mapping[[2]])%>% as.character()
    }
  }
  
  # selected_brush_data
  selected_brush_data <- reactive({   
    validate(need(input$plot_brush, message=FALSE))
     
    brush <- input$plot_brush 
    brushedPoints(mydata, brush, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
  })
  
  # selected_click_data
  selected_click_data <- reactive({
    nearPoints(mydata, input$plot_click, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
  })  
   
  # selected_data
  selected_data <- reactive({                 
    if (nrow(selected_brush_data())>0) {
      selected_brush_data()
    }else if (nrow(selected_click_data())>0) {
      selected_click_data()
    }else {
      NULL
    }
  }) 
  
  # info_container
  output$info_container <- renderUI({
    validate(need(selected_data(), message=FALSE))
    
    output$info <- DT::renderDataTable({ 
      DT::datatable(data = selected_data(),
                    options = list(pageLength = 3, lengthChange = FALSE, width="100%", scrollX = TRUE) 
      ) 
    })
    DT::dataTableOutput(ns("info"))
  })
  
   
  # ggplot_container
  output$ggplot_container <- renderUI({
    output$ggplot <-renderPlot({
       fig + coord_cartesian(xlim = ranges$x, 
                             ylim = ranges$y, 
                             expand = TRUE
                             )
    }#, deleteFile = TRUE 
    #outputArgs = list(brush = brushOpts(id = ns("plot_brush")),
    #                  click = clickOpts(id = ns("plot_click"))
                      #width = "500px",
                      #height = "500px"
    )
    
    plotOutput(ns("ggplot"), 
             brush = brushOpts(id = ns("plot_brush"), 
                               resetOnNew = TRUE),
             click = clickOpts(id = ns("plot_click")),
             # 
             dblclick = dblclickOpts(
                id = ns("plot_dblclick")
                #delay = input$dblclick_delay
              )
              
             # hover = hoverOpts(
             #   id = "plot_hover",
             #   delay = input$hover_delay,
             #   delayType = input$hover_policy,
             #   nullOutside = input$hover_null_outside
             # ),
             # brush = brushOpts(
             #   id = "plot_brush",
             #   delay = input$brush_delay,
             #   delayType = input$brush_policy,
             #   direction = input$brush_dir,
             #   resetOnNew = input$brush_reset
             # )
  )
  })
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
}
