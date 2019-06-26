# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

# WORKED FOR FOUR VARIABLES. 
# ----------------------------------
setwd("~/handbook/")
source("~/handbook/global.R")

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  tagList( 
    fluidRow(
      column(6, 
             plotOutput(ns("plot1"), brush = ns("brush")), 
             tableOutput(ns("summary1"))
             ),
      column(6, 
             plotOutput(ns("plot2"), brush = ns("brush")), 
             tableOutput(ns("summary2"))
             )
    ), 
    
    fluidRow(
      column(6, 
             plotOutput(ns("plot3"), brush = ns("brush")), 
             tableOutput(ns("summary3"))
      ),
      column(6, 
             plotOutput(ns("plot4"), brush = ns("brush")), 
             tableOutput(ns("summary4"))
      )
    )
 ) 
}
 


linkedScatter <- function(input, output, session, 
                          dataset, 
                          xvar_name = "NTIM", 
                          yvar_name = "DVOR", 
                          test_name = "") {
  
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  
  ns <- session$ns
  
  dataWithSelection <- reactive({
    
    validate(need(dataset, message="no dataset found"))
    
    tdata = brushedPoints(dataset, input$brush, allRows = TRUE)%>% 
      mutate(HIGHLIGHT_ID=FALSE, 
             HIGHLIGHT_ID_NTIM = FALSE,
             USUBJID_NTIM = ""
      ) %>% 
      mutate_(NTIM = xvar_name, 
              DVOR = yvar_name
      )
    
    if(!is.null(input$brush)) {
      tdata = switch(
        input$brush$outputId,
          "scatters-plot1" = dataWithSelection_from_which_panel(tdata, which_var=test_name[1]),
          "scatters-plot2"= dataWithSelection_from_which_panel(tdata, which_var=test_name[2]),
          "scatters-plot3" = dataWithSelection_from_which_panel(tdata, which_var=test_name[3]),
          "scatters-plot4"= dataWithSelection_from_which_panel(tdata, which_var=test_name[4]),
        NULL
      )
    }
      
    tdata
  })
  

  output$plot1 <- renderPlot({
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==test_name[1]), 
                cols=c("NTIM", "DVOR")) + 
      scale_y_log10()
  })
  
  output$plot2 <- renderPlot({ 
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==test_name[2]), 
                cols=c("NTIM", "DVOR")) 
  })
  
  output$plot3 <- renderPlot({
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==test_name[3]), 
                cols=c("NTIM", "DVOR"))
  })
  
  output$plot4 <- renderPlot({ 
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==test_name[4]), 
                cols=c("NTIM", "DVOR")) 
  })
  
  output$summary1 <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
   
    output$table_output1 <- renderTable(
      dplyr::filter(dataWithSelection(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== test_name[1] ###
                    )%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
      na = "")
    tableOutput(ns('table_output1'))
  })
  
  
  output$summary2 <- renderUI({ 
    output$table_output2 <- renderTable(
      dplyr::filter(dataWithSelection(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== test_name[2]  ###
                    )%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
      na = "")
    
    tableOutput(ns('table_output2'))
  })
  
  
  output$summary3 <- renderUI({ 
    output$table_output3 <- renderTable(
      dplyr::filter(dataWithSelection(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== test_name[3]  ###
      )%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
      na = "")
    
    tableOutput(ns('table_output3'))
  })
  
  output$summary4 <- renderUI({ 
    output$table_output4 <- renderTable(
      dplyr::filter(dataWithSelection(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== test_name[4]  ###
      )%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
      na = "")
    
    tableOutput(ns('table_output4'))
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
  linkedScatterUI("scatters")

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
    filter(TESTCD %in% c("total dupilumab", "EASI", "NRS" , "IGA"))
  
  
  # "IL-13"           "NRS"             "EASI"            "IGA"             "total dupilumab"
  
  #nmdat <- reactiveValues({
  #  nmdat = nmdat
  #})
  df <- callModule(linkedScatter, "scatters", 
                   dataset = (nmdat),
                   xvar_name = "NTIM", 
                   yvar_name = "DVOR", 
                   test_name = c("total dupilumab", "EASI", "NRS" , "IGA")
                   #left = reactive(c("total dupilumab")),
                   #right = reactive(c("EASI"))
  )
  


}

shinyApp(ui, server)