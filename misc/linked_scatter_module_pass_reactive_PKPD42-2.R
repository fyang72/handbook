# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

# WORKED FOR FOUR VARIABLES. 
# ----------------------------------
setwd("~/handbook/")
source("~/handbook/global.R")

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)


#######################################################################
# linked_profiles_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

linked_profiles_UI <- function(id) {
  ns <- NS(id)
   
  uiOutput(ns("multi_panel"))
 
}



########################################################################
# linked_profiles
########################################################################
linked_profiles <- function(input, output, session, 
                          dataset, 
                          xvar_name = "NTIM", 
                          yvar_name = "DVOR", 
                          test_name = "",  
                          
                          id_name = "USUBJID", 
                          dosegrp_name = "ARMA",
                          testvar_name = "TESTCD", 
                          log_scale = TRUE
                          ) {
  
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  ns <- session$ns
   
  if (length(log_scale)==1) {
    log_scale = rep(log_scale, times=length(test_name))
  }
  
  validate(need(length(log_scale)==length(test_name), message= "log_scale must have same length of test_name"))
  
  #
  output$multi_panel <- renderUI({
     lapply(1:length(test_name), function(i) {
     # lapply(1:length(avec[which(avec)]), function(i) {
      
      tagList(
        #fluidRow(column(6, 
           plotOutput(ns(paste0("plot", i)), brush = ns("brush")), 
           tableOutput(ns(paste0("summary", i)))
        #))
      )
      
    })
  
  })
  
    
  lapply(1:length(test_name), function(i) {
     
      output[[paste0("plot", i)]] <- renderPlot({ 
          
        fig <- scatterPlot(dataWithSelection()  %>% 
                      #filter(TESTCD==test_name[ceiling(i/nextent)]), 
                      filter(TESTCD==test_name[i]), 
                    cols=c("NTIM", "DVOR"))  
        
        if (log_scale[i]) {fig <- fig + scale_y_log10()}
        
        fig
      })
   
    
    output[[paste0("summary", i)]] <- renderUI({
     
      output[[paste0("table_output", i)]] <- renderTable(
        
        dplyr::filter(dataWithSelection(), 
                      HIGHLIGHT_ID_NTIM, 
                      HIGHLIGHT_ID, 
                      #TESTCD== test_name[ceiling(i/nextent)] ###
                      TESTCD== test_name[i] ###
        )%>% 
          select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
        na = "")
      
      tableOutput(ns(paste0("table_output", i)))
    })
    
    #ii = ii + 1
    
    
    #} # if plot+table
  })
   
  
  #----------------------------------
  # reactive of dataWithSelection
  #----------------------------------
  dataWithSelection <- reactive({
    
    validate(need(dataset, message="no dataset found"))
     
    tdata = brushedPoints(dataset, input$brush, allRows = TRUE) %>% 

      mutate_(NTIM = xvar_name, 
              DVOR = yvar_name, 
              USUBJID = id_name,
              ARMA = dosegrp_name,
              TESTCD = testvar_name
      ) %>% 
      
      mutate(
        HIGHLIGHT_ID=FALSE, 
        HIGHLIGHT_ID_NTIM = FALSE,
        USUBJID_NTIM = ""
      )  
    
    if(!is.null(input$brush)) {
      # tdata = switch(
      #   input$brush$outputId,
      #   "profiles-plot1" = dataWithSelection_from_which_panel(tdata, which_var=test_name[1]),
      #   "profiles-plot2"= dataWithSelection_from_which_panel(tdata, which_var=test_name[2]),
      #   "profiles-plot3" = dataWithSelection_from_which_panel(tdata, which_var=test_name[3]),
      #   "profiles-plot4"= dataWithSelection_from_which_panel(tdata, which_var=test_name[4]),
      #   NULL
      # )
      
      tdata <- sapply(1:length(test_name), function(i) {
        if (input$brush$outputId == paste0("profiles-plot", i)) {
          dataWithSelection_from_which_panel(tdata, which_var=test_name[i])
        }
      })  %>% bind_cols()

    }
    
    tdata
  })
  
  return(dataWithSelection)
}


 

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
  
  # ------------------------------------------
  # key varaibles: 
  # USUBJID,  NTIM, TESTCD, DVOR
  # ------------------------------------------
  
  tdata = data #%>% mutate(NSIZE=ifelse(HIGHLIGHT_ID_NTIM, 3, 1))
  # tdata <- tdata%>% 
  #   mutate_(xvar = (cols[1]), 
  #           yvar = (cols[2]) 
  #   )
  
  validate(need(tdata, message=FALSE))
  
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


#########################################################
# 
#########################################################

ui <- fixedPage(
  h2("Module example"),
  linked_profiles_UI("profiles")
  
)

server <- function(input, output, session) {
  #nmdat = read_csv("H:\\QP Portal\\dataset\\nmdatPKPD_ORG.csv")
  
  nmdat = read_csv("~/handbook/data/nmdatPKPD_ORG.csv")
  colnames(nmdat) = toupper(colnames(nmdat)) 
  nmdat = nmdat %>% mutate(ARMA = DOSEGRP)
  
  nmdat = nmdat %>% select(USUBJID, ARMA,TIME,NTIM, TESTCD, DVOR) %>% 
    mutate(TIME=as.numeric(TIME),
           NTIM=as.numeric(NTIM),
           DVOR=as.numeric(DVOR)
    ) %>% 
    filter(TESTCD %in% c("total dupilumab", "EASI", "NRS" , "IGA"))
  
   
  
  #nmdat <- reactiveValues({
  #  nmdat = nmdat
  #})
  df <- callModule(linked_profiles, "profiles", 
                   dataset = (nmdat),
                   xvar_name = "NTIM", 
                   yvar_name = "DVOR", 
                   test_name = c("total dupilumab", "EASI", "NRS" , "IGA"),
                   
                   id_name = "USUBJID", 
                   dosegrp_name = "ARMA",
                   testvar_name = "TESTCD", 
                   log_scale = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  
  
}

shinyApp(ui, server)