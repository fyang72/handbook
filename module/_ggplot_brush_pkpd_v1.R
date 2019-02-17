

ggplot_brush_pkpd_Input <- function(id, label="") {
  
  ns <- NS(id)
  tagList(
     #h4("NTIM vs TIME"),
     uiOutput(ns("ggplot1_container")),
     
     uiOutput(ns("ggplot2_container")),

     # h4("Selected Data"),
     uiOutput(ns("info_container"))
  )
}

 



################################################################################ 
# log in and load data (sidebar)
################################################################################

ggplot_brush_pkpd <- function(input, output, session, 
                              fig1, mydata1, xvar1="NTIM", yvar1="TIME",
                              fig2, mydata2, xvar2="NTIM", yvar2="TIME") {
  
  ns <- session$ns

  # only for internal use
  values <- reactiveValues(which_active= "fig1"
  )
  
  #################
  # fig1
  #################
  # selected_brush_data
  fig1_selected_brush_data <- reactive({                 
    brushedPoints(mydata1, input$plot1_brush, xvar1, yvar1, allRows = FALSE)  %>% as.data.frame()
  })
  
  # selected_click_data
  fig1_selected_click_data <- reactive({
    nearPoints(mydata1, input$plot1_click, xvar1, yvar1, allRows = FALSE)  %>% as.data.frame()
  })  
   
  # selected_data
  fig1_selected_data <- reactive({   
    values$which_active = "fig1"
    print('after_fig1_')
    print(values$which_active)
    
    if (nrow(fig1_selected_brush_data())>0) {
      fig1_selected_brush_data()
    }else if (nrow(fig1_selected_click_data())>0) {
      fig1_selected_click_data()
    }else {
      NULL
    }
     
  }) 
  
  
  #################
  # fig2
  #################
  # selected_brush_data
  fig2_selected_brush_data <- reactive({                 
    brushedPoints(mydata2, input$plot2_brush, xvar2, yvar2, allRows = FALSE)  %>% as.data.frame()
  })
  
  # selected_click_data
  fig2_selected_click_data <- reactive({
    nearPoints(mydata2, input$plot2_click, xvar2, yvar2, allRows = FALSE)  %>% as.data.frame()
  })  
  
  # selected_data
  fig2_selected_data <- reactive({    
    values$which_active = "fig2"
    print('after_fig2_')
    print(values$which_active)
    
    if (nrow(fig2_selected_brush_data())>0) {
      fig2_selected_brush_data()
    }else if (nrow(fig2_selected_click_data())>0) {
      fig2_selected_click_data()
    }else {
      NULL
    }
    
  }) 
  
  # selected_data
  selected_data <- reactive({    
    #validate(need(fig1_selected_data(), message=FALSE), 
    #        need(fig2_selected_data(), message=FALSE)
    #)
    validate(need(values$which_active, message= "no active plot..."))
    
    if (values$which_active == "fig1") {
      fig1_selected_data()
    }else if (values$which_active == "fig2") {
      fig2_selected_data()
    }else {
      NULL
    }
    
    # if (nrow(fig1_selected_data())>0) {
    #   fig1_selected_data()
    # }else if (nrow(fig2_selected_data())>0) {
    #   fig2_selected_data()
    # }else {
    #   NULL
    # }
    
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
  output$ggplot1_container <- renderUI({
    output$ggplot1 <-renderPlot({
       #fig1  # ggplot object
       if (1==2) {  #(nrow(fig2_selected_data())>0) {
         fig1 = fig1 + ggplot(data=fig2_selected_data(), 
                              aes(x=xvar1, y=yvar1)) + 
                              geom_point(color="red") + 
                              geom_line(color="blue",lwd=2)
            
       }
        
        fig1
    }, deleteFile = TRUE 
    #outputArgs = list(brush = brushOpts(id = ns("plot_brush")),
    #                  click = clickOpts(id = ns("plot_click"))
                      #width = "500px",
                      #height = "500px"
    )
    
    plotOutput(ns("ggplot1"), 
             brush = brushOpts(id = ns("plot1_brush")),
             click = clickOpts(id = ns("plot1_click"))
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



# ggplot_container
output$ggplot2_container <- renderUI({
  output$ggplot2 <-renderPlot({
    #fig2  # ggplot object
    if (1==2) {  #nrow(fig1_selected_data())>0) {
      fig2 = fig2 + ggplot(data=fig1_selected_data(), 
                           aes(x=xvar2, y=yvar2)) + 
        geom_point(color="red") + 
        geom_line(color="blue",lwd=2)
      
    }
    
    fig2
  }, deleteFile = TRUE  
  )
  
  plotOutput(ns("ggplot2"), 
             brush = brushOpts(id = ns("plot2_brush")),
             click = clickOpts(id = ns("plot2_click"))
             
  )
})

}


