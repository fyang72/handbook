# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
  )
}

linkedScatter <- function(input, output, session, data, left, right) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  
  dataWithSelection <- reactive({
    
    tdata = brushedPoints(data(), input$brush, allRows = TRUE)%>% 
      mutate(HIGHLIGHT_ID=FALSE, 
             HIGHLIGHT_ID_NTIM = FALSE
      )
    
    # print(input$brush)
    # if(!is.null(input$brush)) {
    # 
    #   
    #   if(input$brush$outputId == "scatters-plot2") {
    #     tdata = tdata %>% filter(TESTCD==right())
    #   }
    # }
    
    
    tdata
  })
  
  
  dataWithSelection_from_left <- reactive({
    
    tdata = dataWithSelection()
    
    subj_lst = tdata %>% filter(selected_, TESTCD==left()) %>%
      pull(USUBJID) %>% unique()
    
    NTIM_lst = tdata %>% filter(selected_) %>% 
      pull(NTIM) %>% unique()
    
    tdata = tdata %>% mutate(
      HIGHLIGHT_ID = ifelse(USUBJID %in% subj_lst, TRUE, FALSE), 
      HIGHLIGHT_ID_NTIM = ifelse((USUBJID %in% subj_lst) & (NTIM %in% NTIM_lst), TRUE, FALSE)
    )
    
    print("highlighted USUBJID:")
    print(tdata %>% filter(HIGHLIGHT_ID) %>% pull(USUBJID) %>% unique() )
    
    tdata
  })
  
  
  
  output$plot1 <- renderPlot({
    tdata = dataWithSelection()  %>% filter(TESTCD==left())
    
    if(!is.null(input$brush) && input$brush$outputId == "scatters-plot1") {
      print("scatters-plot1")
      tdata = dataWithSelection_from_left()  %>% filter(TESTCD==left())
    } 
    
    scatterPlot(tdata, cols=c("NTIM", "DVOR"))+ scale_y_log10()
  })
  
  output$plot2 <- renderPlot({
    tdata = dataWithSelection()  %>% filter(TESTCD==right())
    
    if(!is.null(input$brush) &&  input$brush$outputId == "scatters-plot1") {
      print("scatters-plot1-plot2")
      tdata = dataWithSelection_from_left()  %>% filter(TESTCD==right())
    }
    
    scatterPlot(tdata, cols=c("NTIM", "DVOR"))
  })
  
  return(dataWithSelection_from_left)
}


scatterPlot <- function(data, cols) {
  ggplot(data, aes_string(x = cols[1], y = cols[2], group="USUBJID")) +
    geom_point(aes(color = selected_)) +
    geom_line(aes(color = HIGHLIGHT_ID)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}

ui <- fixedPage(
  h2("Module example"),
  linkedScatterUI("scatters"),
  fluidRow(
    column(6, tableOutput(("summary1") )),
    column(6, tableOutput(("summary2") ))
  )
)

server <- function(input, output, session) {
  #nmdat = read_csv("H:\\QP Portal\\dataset\\nmdatPKPD_ORG.csv")
  
  nmdat = read_csv("~/handbook/data/nmdatPKPD_ORG.csv")
  colnames(nmdat) = toupper(colnames(nmdat)) 
  nmdat = nmdat %>% select(STUDYID, USUBJID, TIME,NTIM, TESTCD, DVOR) %>% 
    mutate(TIME=as.numeric(TIME),
           NTIM=as.numeric(NTIM),
           DVOR=as.numeric(DVOR)
    ) %>% 
    filter(TESTCD %in% c("EASI", "total dupilumab"))
  
  df <- callModule(linkedScatter, "scatters", 
                   data = reactive(nmdat),
                   left = reactive(c("total dupilumab")),
                   right = reactive(c("EASI"))
  )
  
  output$summary1 <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
    
    output$table_output1 <- renderTable(dplyr::filter(df(), HIGHLIGHT_ID_NTIM, HIGHLIGHT_ID, TESTCD== "total dupilumab"), #%>% 
                                        #select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
                                        na = "")
    tableOutput(('table_output1'))
    
  })
  
  output$summary2 <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
    
    output$table_output2 <- renderTable(dplyr::filter(df(), HIGHLIGHT_ID_NTIM, HIGHLIGHT_ID, TESTCD== "EASI"), #%>% 
                                        #select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
                                        na = "")
    tableOutput(('table_output2'))
    
  })
}

shinyApp(ui, server)