

ggplot_brush_Input <- function(id, label="") {
  
  ns <- NS(id)
  tagList(
     #h4("NTIM vs TIME"),
     uiOutput(ns("ggplot_container")),

     # h4("Selected Data"),
     uiOutput(ns("info_container"))
  )
}


ggplot_brush_Input2 <- function(id, label="") {
  
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

ggplot_brush <- function(input, output, session, fig, mydata, xvar="NTIM", yvar="TIME") {
  
  ns <- session$ns

  # selected_brush_data
  selected_brush_data <- reactive({                 
    brushedPoints(mydata, input$plot_brush, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
  })
  
  # selected_click_data
  selected_click_data <- reactive({
    nearPoints(mydata, input$plot_click, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
  })  
   
  # selected_data
  selected_data <- reactive({                 
    # if (nrow(selected_brush_data())>0) {
    #   selected_brush_data()
    # }else if (nrow(selected_click_data())>0) {
    #   selected_click_data()
    # }else {
    #   NULL
    # }
     
    t1 = brushedPoints(mydata, input$plot_brush, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
    t2 = nearPoints(mydata, input$plot_click, xvar, yvar, allRows = FALSE)  %>% as.data.frame()
    
    outdata = NULL
    if (!is.null(t2) && nrow(t2)>0) {outdata = t2}
    if (!is.null(t1) && nrow(t1)>0) {outdata = t1 }

    outdata
    
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
       #fig  # ggplot object
      
        tdata =  (selected_data())
        if (!is.null(tdata)&&nrow(selected_data())>0) {  #!is.null(tdata)) { #
         fig = fig + 
           geom_point(data=mydata %>% filter(USUBJID %in% unique(tdata$USUBJID)), color="red") #+ 
           #geom_line(data=mydata %>% filter(USUBJID %in% unique(tdata$USUBJID)), color="blue", lwd=2)
         
       }
       fig
      
    }, deleteFile = TRUE 
    #outputArgs = list(brush = brushOpts(id = ns("plot_brush")),
    #                  click = clickOpts(id = ns("plot_click"))
                      #width = "500px",
                      #height = "500px"
    )
    
    plotOutput(ns("ggplot"), 
             brush = brushOpts(id = ns("plot_brush")),
             click = clickOpts(id = ns("plot_click"))
             # 
             # dblclick = dblclickOpts(
             #   id = "plot_dblclick",
             #   delay = input$dblclick_delay
             # ),
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
}
