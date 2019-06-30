
#######################################################################
# linked_profiles_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_linked_profiles_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("multi_panel"))
  
}



########################################################################
# linked_profiles
########################################################################
module_linked_profiles <- function(input, output, session, 
                            dataset, 
                            xvar_name = "NTIM", 
                            xvar_name_label = xvar_name,
                            
                            yvar_name = "DVOR", 
                            test_name = "",  
                            test_name_label = test_name, 
                            log_scale = TRUE, 
                            
                            id_name = "USUBJID", 
                            dosegrp_name = "ARMA",
                            testvar_name = "TEST" 
                            
) {
  
  # data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  # data frame with an additional columns "HIGHLIGHT_ID" and "HIGHLIGHT_ID_NTIM
  # that indicates whether that observation is brushed, USUBJID/NTIM highlighed
  ns <- session$ns
  
  xvar_name = xvar_name()
  xvar_name_label = xvar_name_label()
  yvar_name = yvar_name()
  
  test_name = test_name()
  test_name_label = test_name_label()
  log_scale = log_scale()
  id_name = id_name()
  
  dosegrp_name = dosegrp_name()
  testvar_name = testvar_name()
  
  if (length(log_scale)==1) {
    log_scale = rep(log_scale, times=length(test_name))
  }
  
  log_scale = log_scale[1:length(test_name)]
  #validate(need(length(log_scale)==length(test_name), 
  #              message= "log_scale must have same length of test_name"))
  
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
      
      validate(need(dataWithSelection(), message="no data found"))
      
      fig <- linked_time_profile_plot(
        dataWithSelection() %>% filter(TEST==test_name[i]),
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
    
    # 
    # output[[paste0("summary", i)]] <- renderUI({
    #   
    #   validate(need(dataWithSelection(), message="no data found"))
    #    
    #   output[[paste0("table_output", i)]] <- renderTable(
    #     
    #     dplyr::filter(dataWithSelection(), 
    #                   HIGHLIGHT_ID_NTIM, 
    #                   HIGHLIGHT_ID, 
    #                   TEST== test_name[i] ###
    #     )%>% 
    #       select(-selected_, -HIGHLIGHT_ID, -HIGHLIGHT_ID_NTIM, -USUBJID_NTIM, -xvar, -yvar), 
    #     na = "")
    #   
    #   tableOutput(ns(paste0("table_output", i)))
    # })
    # 
      
    output[[paste0("summary", i)]] <- renderUI({
      
      validate(need(dataWithSelection(), message="no data found"))
      
      output[[paste0("table_output", i)]] <- DT::renderDataTable(                                                                                                                                          
        DT::datatable(data = 
                        dplyr::filter(dataWithSelection(), 
                                     HIGHLIGHT_ID_NTIM, 
                                     HIGHLIGHT_ID, 
                                     TEST== test_name[i] ###
        ),                                                                                                                                                       
          options = list(pageLength = input$pageLength, 
                         lengthChange = FALSE, width="100%", scrollX = TRUE)                                                                   
        ))
      
      DT::dataTableOutput(ns(paste0("table_output", i)))
    })
    
    
  })
   
  
  
  #----------------------------------
  # reactive of dataWithSelection
  #----------------------------------
  dataWithSelection <- reactive({
    
    validate(need(dataset, message="no dataset found"), 
             need(test_name, message="no analyte variables found")
             )
    
    tdata <- dataset %>% 
      mutate_(xvar = xvar_name, 
              yvar = yvar_name 
      )  %>% 
      mutate(xvar=as_numeric(xvar),
             yvar=as_numeric(yvar)
      ) 
    
    # add "selected_"
    tdata = brushedPoints(tdata, input$brush, allRows = TRUE) %>% 
      
      mutate_(NTIM = xvar_name, 
              DVOR = yvar_name, 
              USUBJID = id_name,
              ARMA = dosegrp_name,
              TEST = testvar_name
      ) %>% 

      mutate(
        HIGHLIGHT_ID=FALSE, 
        HIGHLIGHT_ID_NTIM = FALSE,
        USUBJID_NTIM = ""
      )  
    
    if(!is.null(input$brush)) {
      
      which_panel_brushed <- str_split(input$brush$outputId, "-") %>% unlist() %>% last()
      tdata1 <- sapply(1:length(test_name), function(i) {
        if (which_panel_brushed == paste0("plot", i)) {
          dataWithSelection_from_which_panel(tdata, which_var=test_name[i])
        }
      })  %>% bind_cols()
      
      colnames(tdata1) <- colnames(tdata)  # seems lost colnames 06/29/2019
      tdata = tdata1
    }
    
    tdata
  })
  
  
  #------------------------------------------------------
  # util function of dataWithSelection_from_which_panel
  #------------------------------------------------------
  dataWithSelection_from_which_panel <- function(tdata, which_var=NULL) {
    validate(need(tdata, message="no data found"))
    
    subj_lst = tdata %>% filter(selected_, TEST==which_var) %>%
      pull(USUBJID) %>% unique()
    
    USUBJID_NTIM_lst = tdata %>% filter(selected_, TEST==which_var) %>% 
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
    # USUBJID, ARMA, NTIM, TEST, DVOR, WGTBL
    # ------------------------------------------
    
    tdata = data
    validate(need(tdata, message=FALSE))
    
    x=setup_scale(myscale='1_7', mylimit=c(0, max(tdata$xvar, na.rm=TRUE)))
    
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