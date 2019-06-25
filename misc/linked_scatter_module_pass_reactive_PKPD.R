# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

setwd("~/handbook/")
source("~/handbook/global.R")

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
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- reactive({
    tdata = brushedPoints(data(), input$brush, allRows = TRUE)%>% 
      mutate(HIGHLIGHT_ID=FALSE, 
             HIGHLIGHT_ID_NTIM = FALSE
      )
    tdata
  })
  
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  
  # user brush the left panel
  dataWithSelection_from_left <- reactive({
    tdata = dataWithSelection()
    
    subj_lst = tdata %>% filter(selected_, TESTCD==left()) %>%
      pull(USUBJID) %>% unique()
    
    NTIM_lst = tdata %>% filter(selected_, TESTCD==left()) %>% 
      pull(NTIM) %>% unique()
    
    tdata = tdata %>% mutate(
      HIGHLIGHT_ID = ifelse(USUBJID %in% subj_lst, TRUE, FALSE), 
      HIGHLIGHT_ID_NTIM = ifelse( (USUBJID %in% subj_lst) & (NTIM %in% NTIM_lst), TRUE, FALSE)
    )
    
    print(tdata %>% filter(HIGHLIGHT_ID_NTIM))
    
    tdata
  })
  
  # user brush the right panel
  dataWithSelection_from_right <- reactive({
    tdata = dataWithSelection()
    
    subj_lst = tdata %>% filter(selected_, TESTCD==right()) %>%
      pull(USUBJID) %>% unique()
    
    NTIM_lst = tdata %>% filter(selected_) %>% 
      pull(NTIM) %>% unique()
    
    tdata = tdata %>% mutate(
      HIGHLIGHT_ID = ifelse(USUBJID %in% subj_lst, TRUE, FALSE), 
      HIGHLIGHT_ID_NTIM = ifelse((USUBJID %in% subj_lst) & (NTIM %in% NTIM_lst), TRUE, FALSE)
    )
    
    tdata
  })
  
  output$plot1 <- renderPlot({
    # default
    tdata = dataWithSelection()  %>% filter(TESTCD==left())
    
    # if burshed from left
    if(!is.null(input$brush) && input$brush$outputId == "scatters-plot1") {
      tdata = dataWithSelection_from_left()  %>% filter(TESTCD==left())
    } 
    
    # if burshed from right
    if(!is.null(input$brush) &&  input$brush$outputId == "scatters-plot2") {
      tdata = dataWithSelection_from_right()  %>% filter(TESTCD==left())
    }
    
    scatterPlot(tdata, cols=c("NTIM", "DVOR")) + scale_y_log10()
  })
  
  output$plot2 <- renderPlot({
    # default
    tdata = dataWithSelection()  %>% filter(TESTCD==right())
    
    # if burshed from left
    if(!is.null(input$brush) &&  input$brush$outputId == "scatters-plot1") {
      tdata = dataWithSelection_from_left()  %>% filter(TESTCD==right())
    }
    
    # if burshed from right
    if(!is.null(input$brush) &&  input$brush$outputId == "scatters-plot2") {
      tdata = dataWithSelection_from_right()  %>% filter(TESTCD==right())
    }
    
    scatterPlot(tdata, cols=c("NTIM", "DVOR"))
  })
  
  return(list(dataWithSelection_from_left, 
              dataWithSelection_from_right, 
              reactive(input$brush)
              )
         )
}


scatterPlot <- function(data, cols) {
  tdata = data #%>% mutate(NSIZE=ifelse(HIGHLIGHT_ID_NTIM, 3, 1))
  # tdata <- tdata%>% 
  #   mutate_(xvar = (cols[1]), 
  #           yvar = (cols[2]) 
  #   )
  
  tdata = tdata %>% as.data.frame()
  #tdata[, cols[1]] = tdata[, cols[1]]/7
  #tdata$yvar = tdata[, cols[2]]
  #print(tdata)
  
  #x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata[, cols[1]], na.rm=TRUE) ))
  
  ggplot(tdata, aes_string(x = cols[1], y = cols[2], group="USUBJID")) +
    geom_point(aes(color = HIGHLIGHT_ID_NTIM)) +
    geom_point(data=tdata%>%filter(HIGHLIGHT_ID_NTIM), 
               aes(color = HIGHLIGHT_ID_NTIM),
               size=6) +
    
    geom_line(aes(color = HIGHLIGHT_ID)) +
    geom_line(data=tdata%>%filter(HIGHLIGHT_ID), 
               aes(color = HIGHLIGHT_ID),
               lwd=1) + 
    #scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
  
  
     scale_color_manual(values=colScheme(), guide = FALSE) + 
    # 
    # scale_x_continuous(breaks=x$breaks, label=x$labels) +
    # #scale_y_continuous(breaks=y$breaks, label=y$labels) +
    # 
    # #coord_cartesian(xlim = c(0, 85)) + 
    # 
     xlab("Time (week)") +  
     ylab(paste0("Concentration (mg/L)")) +   
     
     theme_bw() + base_theme(font.size = as.integer(12)) #+ 
     #guides(col=guide_legend(ncol=4,byrow=TRUE))  
  
  
  
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
    
    brush = df[[3]]()
    
    dff = df[[1]]
    if(!is.null(brush) && brush$outputId == "scatters-plot2") {
      dff = df[[2]]
    } #else if (brush$outputId == "scatters-plot2") {
      #dff = df[[2]]
    #}
    
    output$table_output1 <- renderTable(
      dplyr::filter(dff(), HIGHLIGHT_ID_NTIM, HIGHLIGHT_ID, TESTCD== "total dupilumab")%>% 
      select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
                                        na = "")
    tableOutput(('table_output1'))
    
  })
  
  output$summary2 <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
    brush = df[[3]]()
    
    dff = df[[1]]
    if(!is.null(brush) && brush$outputId == "scatters-plot2") {
      dff = df[[2]]
    } #else if (brush$outputId == "scatters-plot1") {
    
    #}
  
    output$table_output2 <- renderTable(
      dplyr::filter(dff(), HIGHLIGHT_ID_NTIM, HIGHLIGHT_ID, TESTCD== "EASI")%>% 
                                        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
                                        na = "")
    tableOutput(('table_output2'))
    
  })
}

shinyApp(ui, server)