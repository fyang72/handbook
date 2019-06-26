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
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  dataWithSelection <- reactive({
    tdata = brushedPoints(data(), input$brush, allRows = TRUE)%>% 
      mutate(HIGHLIGHT_ID=FALSE, 
             HIGHLIGHT_ID_NTIM = FALSE
      ) 
    
    if(!is.null(input$brush)) {
      tdata = switch(
        input$brush$outputId,
          "scatters-plot1" = dataWithSelection_from_which_panel(tdata, which_var=left()),
          "scatters-plot2"= dataWithSelection_from_which_panel(tdata, which_var=right()),
        NULL
      )
    }
      
    tdata
  })
  

  output$plot1 <- renderPlot({
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==left()), 
                cols=c("NTIM", "DVOR")) + 
      scale_y_log10()
  })
  
  output$plot2 <- renderPlot({ 
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==right()), 
                cols=c("NTIM", "DVOR")) 
  })
  
  return(dataWithSelection)
}



#tdata = dataWithSelection()

dataWithSelection_from_which_panel <- function(tdata, which_var=NULL) {
  subj_lst = tdata %>% filter(selected_, TESTCD==which_var) %>%
    pull(USUBJID) %>% unique()
  
  USUBJID_NTIM_lst = tdata %>% filter(selected_, TESTCD==which_var) %>% 
    mutate(USUBJID_NTIM = paste0(USUBJID, "-", NTIM))    %>% 
    pull(USUBJID_NTIM) %>% unique()
  
  tdata = tdata %>% 
    mutate(USUBJID_NTIM = paste0(USUBJID, "-", NTIM))    %>% 
    mutate(
      HIGHLIGHT_ID = ifelse(USUBJID %in% subj_lst, TRUE, FALSE), 
      HIGHLIGHT_ID_NTIM = ifelse(USUBJID_NTIM %in% USUBJID_NTIM_lst, TRUE, FALSE)
    )
  tdata
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
    
    # brush = df[[3]]()
    # 
    # dff = df[[1]]
    # if(!is.null(brush) && brush$outputId == "scatters-plot2") {
    #   dff = df[[2]]
    # } #else if (brush$outputId == "scatters-plot2") {
    #   #dff = df[[2]]
    # #}
    
    output$table_output1 <- renderTable(
      dplyr::filter(df(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== "total dupilumab")%>% 
      select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
                                        na = "")
    tableOutput(('table_output1'))
    
  })
  
  output$summary2 <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
    # brush = df[[3]]()
    # 
    # dff = df[[1]]
    # if(!is.null(brush) && brush$outputId == "scatters-plot2") {
    #   dff = df[[2]]
    # } #else if (brush$outputId == "scatters-plot1") {
    # 
    # #}
  
    output$table_output2 <- renderTable(
      dplyr::filter(df(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, TESTCD== "EASI")%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM), 
               na = "")
    
    tableOutput(('table_output2'))
    
  })
}

shinyApp(ui, server)