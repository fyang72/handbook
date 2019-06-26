
linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           plotOutput(ns("plot"), brush = ns("brush")), 
           tableOutput(ns("summary"))
    ) 
  ) 
}



linkedScatter <- function(input, output, session, 
                          dataWithSelection, 
                          xvar_name = "NTIM", 
                          yvar_name = "DVOR", 
                          test_name = "", 
                          index = i
                          ) {
  
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  
  ns <- session$ns
  
  
  
  output$plot <- renderPlot({
    scatterPlot(dataWithSelection()  %>% 
                  filter(TESTCD==test_name[1]), 
                cols=c("NTIM", "DVOR")) 
  })
   
  
  output$summary <- renderUI({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    #renderTable(dplyr::filter(df(), selected_), na = "")
    
    output$table_output <- renderTable(
      dplyr::filter(dataWithSelection(), 
                    HIGHLIGHT_ID_NTIM, 
                    HIGHLIGHT_ID, 
                    TESTCD== test_name[1] ###
      )%>% 
        select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM), 
      na = "")
    tableOutput(ns('table_output'))
  })
   
  return(reactive(input$brush))
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
  uiOutput("ALL")
  
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
    filter(TESTCD %in% c("total dupilumab", "EASI", "NRS" , "IGA"))%>% 
    
    mutate(selected_ = FALSE, 
           HIGHLIGHT_ID=FALSE, 
           HIGHLIGHT_ID_NTIM = FALSE,
           USUBJID_NTIM = ""
    )

  # "IL-13"           "NRS"             "EASI"            "IGA"             "total dupilumab"
  
  #nmdat <- reactiveValues({
  #  nmdat = nmdat
  #})
  

  
  
test_name_lst <- c("total dupilumab", "EASI", "NRS" , "IGA")
  
output$ALL <- renderUI({

  brush <- function() {return(NULL)}
  
  dataWithSelection <- reactive({
    
    validate(need(nmdat, message="no dataset found"))
    
    tdata =  nmdat
    #mutate_(NTIM = xvar_name, 
    #        DVOR = yvar_name
    #)
    

    
    if(!is.null(brush())) {
      print("brush()")
      print(brush())
      
      
      tdata = brushedPoints(nmdat, brush(), allRows = TRUE) #%>%  
      #if (brush()$outputId == paste0("scatters",  i, "-plot")) {
        print("within brush: ")
        #tdata = dataWithSelection_from_which_panel(tdata, which_var=test_name_lst[i])
        
        tdata = switch(
          brush()$outputId,
          "scatters1-plot" = dataWithSelection_from_which_panel(tdata, which_var=test_name_lst[1]),
          "scatters2-plot"= dataWithSelection_from_which_panel(tdata, which_var=test_name_lst[2]),
          "scatters3-plot" = dataWithSelection_from_which_panel(tdata, which_var=test_name_lst[3]),
          "scatters4-plot"= dataWithSelection_from_which_panel(tdata, which_var=test_name_lst[4]),
          NULL
        )
        #print(head(tdata))
        
     # }
    }
    
    tdata
  })
  
  lapply(1:length(test_name_lst), function(i) {
    # validate(need(values$table[[i]], message="no table found"), 
    #          need(is.data.frame(values$table[[i]]), message="only data.frame allowed")
    # )
    
    
    
    # save values$table into ALL$TABLE
    brush <- callModule(linkedScatter, paste0("scatters", i), 
                      dataWithSelection,
                     xvar_name = "NTIM", 
                     yvar_name = "DVOR", 
                     test_name = test_name_lst[i], 
                     index = i 
    )
    
    #print("after call module:")
    #print(brush())
    
    linkedScatterUI(paste0("scatters", i) ) 
    
    
  })
  
  
})
 



  

  
  
  
}

shinyApp(ui, server)










