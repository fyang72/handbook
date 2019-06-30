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
                          xvar_name_label = xvar_name,
                          
                          yvar_name = "DVOR", 
                          test_name = "",  
                          test_name_label = test_name, 
                          log_scale = TRUE, 
                          
                          id_name = "USUBJID", 
                          dosegrp_name = "ARMA",
                          testvar_name = "TESTCD" 
                          
                          ) {
  
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  ns <- session$ns
   
  if (length(log_scale)==1) {
    log_scale = rep(log_scale, times=length(test_name))
  }
  
  validate(need(length(log_scale)==length(test_name), 
                message= "log_scale must have same length of test_name"))
  
  #
  output$multi_panel <- renderUI({
     lapply(1:length(test_name), function(i) {

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
          
        fig <- linked_time_profile_plot(
          dataWithSelection() %>% filter(TESTCD==test_name[i]),
          xvar_name_label = xvar_name_label[i], 
          test_name_label = test_name_label[i]
        )  
        
        if (log_scale[i]) {
          fig <- fig + scale_y_log10()  + 
            scale_y_log10(breaks = 10^(seq(-3,3,by=1)),      #trans_breaks("log10", function(x) 10^x),
                          labels = 10^(seq(-3,3,by=1))) +      # trans_format("log10", math_format(10^.x))) +
            annotation_logticks(sides ="l")  #+  # "trbl", for top, right, bottom, and left.
          }
        
        fig
      })
   
    
    output[[paste0("summary", i)]] <- renderUI({
     
      output[[paste0("table_output", i)]] <- renderTable(
        
        dplyr::filter(dataWithSelection(), 
                      HIGHLIGHT_ID_NTIM, 
                      HIGHLIGHT_ID, 
                      TESTCD== test_name[i] ###
        )%>% 
          select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM, -xvar, -yvar), 
        na = "")
      
      tableOutput(ns(paste0("table_output", i)))
    })
    
  })
   
  
  #----------------------------------
  # reactive of dataWithSelection
  #----------------------------------
  dataWithSelection <- reactive({
    
    validate(need(dataset, message="no dataset found"))

    tdata <- dataset %>% 
      mutate_(xvar = xvar_name, 
              yvar = yvar_name 
      )
    
    # add "selected_"
    tdata = brushedPoints(tdata, input$brush, allRows = TRUE) %>% 

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
      
      print("input$brush$outputId")
      print(input$brush$outputId)
      
      which_panel_brushed <- str_split(input$brush$outputId, "-") %>% unlist() %>% last()
      tdata <- sapply(1:length(test_name), function(i) {
        if (which_panel_brushed == paste0("plot", i)) {
          dataWithSelection_from_which_panel(tdata, which_var=test_name[i])
        }
      })  %>% bind_cols()

    }
    
    tdata
  })
  
  
  #------------------------------------------------------
  # util function of dataWithSelection_from_which_panel
  #------------------------------------------------------
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
  
  #------------------------------------------------------
  # util function of linked_time_profile_plot
  #------------------------------------------------------  
  linked_time_profile_plot <- function(data, 
                          xvar_name_label, 
                          test_name_label) {
    
    # ------------------------------------------
    # key varaibles: 
    # USUBJID, ARMA, NTIM, TESTCD, DVOR, WGTBL
    # ------------------------------------------
    
    tdata = data
    validate(need(tdata, message=FALSE))
    
    x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$xvar, na.rm=TRUE)))
    
    ggplot(tdata, aes_string(x = "xvar", y = "yvar", group="USUBJID")) +
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
      scale_x_continuous(breaks=x$breaks, label=x$labels) +
      # #scale_y_continuous(breaks=y$breaks, label=y$labels) +
      # 
      # #coord_cartesian(xlim = c(0, 85)) + 
      # 
      xlab(xvar_name_label) +  
      ylab(test_name_label) +   
      
      theme_bw() + base_theme(font.size = as.integer(12)) #+ 
    #guides(col=guide_legend(ncol=4,byrow=TRUE))  
    
  }

  return(dataWithSelection)
}


 



#########################################################
# 
#########################################################

ui <- fixedPage(
  h2("Module example"),
  module_linked_profiles_UI("profiles")
  
)

server <- function(input, output, session) {
  #nmdat = read_csv("H:\\QP Portal\\dataset\\nmdatPKPD_ORG.csv")
  
  nmdat = read_csv("~/handbook/data/nmdatPKPD_ORG.csv")
  colnames(nmdat) = toupper(colnames(nmdat)) 
  nmdat = nmdat %>% mutate(ARMA = DOSEGRP)
  
  nmdat = nmdat %>% select(USUBJID, ARMA,TIME,NTIM, TESTCD, DVOR, WGTBL) %>% 
    mutate(TIME=as.numeric(TIME),
           NTIM=as.numeric(NTIM),
           DVOR=as.numeric(DVOR)
    ) %>% 
    filter(TESTCD %in% c("total dupilumab", "EASI", "NRS" , "IGA")) %>% 
    mutate(NTIM = as_numeric(NTIM)/7)
   
  
  #nmdat <- reactiveValues({
  #  nmdat = nmdat
  #})
  df <- callModule(module_linked_profiles, "profiles", 
                   dataset = (nmdat),
                   xvar_name = "NTIM", 
                   xvar_name_label = "Time (Week)",
                   
                   yvar_name = "DVOR", 
                   test_name = c("total dupilumab", "EASI", "NRS" , "IGA"),
                   test_name_label = c("total dupilumab", "EASI", "NRS" , "IGA"), 
                   log_scale = c(TRUE, FALSE, FALSE, FALSE),
                   
                   id_name = "USUBJID", 
                   dosegrp_name = "ARMA",
                   testvar_name = "TESTCD"
                   
  )
  
  
  
}

shinyApp(ui, server)