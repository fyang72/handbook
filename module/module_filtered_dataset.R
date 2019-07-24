


################################################################################ 
# module_filtered_dataset_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_filtered_dataset_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  #fluidRow(
  
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    # filters
    # HTML(colFmt("Step 2: Apply following filter(s), if needed, to narrow down the dataset<br>", color="darkblue")
    # ),
    #fluidRow(column(width=12,
  tagList(
      fluidRow(column(width=6, uiOutput(ns("select_STUDYID_container")))), 
      fluidRow(column(width=6, uiOutput(ns("select_TEST_container")))), 
      fluidRow(column(width=12, uiOutput(ns("select_ARMA_container"))))
    )#) #,
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    
    # HTML(colFmt("Step 3: Review the (filtered) dataset <br>", color="darkblue")
    # ),
    # uiOutput(ns("filtered_dataset_container")), 
    #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    
  #) 
  
}



################################################################################ 
# main function: module_filtered_dataset
################################################################################

module_filtered_dataset <- function(input, output, session, 
                                    dataset
)  {
  
 
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL, table = NULL)
  
  ################################
  # Load dataset
  ################################ 
  load_dataset <- reactive({
    dataset
    
  })
  
  filtered_dataset <- reactive({
    
    dataset = load_dataset()
    validate(need(dataset, message="no dataset"))
    
    if (class(dataset) == "xpose.data") {
      tdata = dataset %>% slot("Data")
    }else if (class(dataset) == "list"  & all(c("adsl", "adex", "adpc") %in% names(dataset))) {
      tdata = dataset[["adsl"]]
    }else {
      tdata = dataset
    }
    
    if ("STUDYID" %in% colnames(tdata) && !is.null(input$which_studyid)) {
      tdata = tdata %>% filter(STUDYID %in% input$which_arma)
    } 
    
    if ("TEST" %in% colnames(tdata) && !is.null(input$which_test)) {
      tdata = tdata %>% filter(TEST %in% input$which_test)
    }
    
    if ("ARMA" %in% colnames(tdata) && !is.null(input$which_arma)) {
      tdata = tdata %>% filter(ARMA %in% input$which_arma)
    } 
    
    tdata
  })
  
  
  
  ################################
  # UI for dataset_container
  ################################ 
    
  
  output$select_STUDYID_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE),  
             need("STUDYID" %in% colnames(tdata), message=FALSE) 
    )
    
    studyid_lst = c(unique(tdata%>%drop_na(STUDYID)%>%pull(STUDYID))) 
    validate(need(length(studyid_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    # column(6,
    inline(
      selectizeInput(ns("which_studyid"), 
                     label    = "select which study", 
                     choices  = studyid_lst, 
                     multiple = TRUE,
                     width="100%", 
                     selected = studyid_lst[1]
      )
    )
    # )
    # )
    
  })
  
  
  output$select_TEST_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE),  
             need("TEST" %in% colnames(tdata), message=FALSE) 
    )
    
    test_lst = c(unique(tdata%>%drop_na(TEST)%>%pull(TEST))) 
    validate(need(length(test_lst)>1, message=FALSE))  
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    #fluidRow(
    # column(6,
    inline(
      selectizeInput(ns("which_test"), 
                     label    = "select which analyte", 
                     choices  = test_lst, 
                     multiple = FALSE,
                     width="100%", 
                     selected = test_lst[1]
      )
    )
    # )
    # )
    
  })
  
  
  output$select_ARMA_container <- renderUI({ 
    tdata = load_dataset()
    validate(need(tdata, message=FALSE), 
             need("ARMA" %in% colnames(tdata), message=FALSE) 
    )
    
    arma_lst = c(unique(tdata%>%drop_na(ARMA)%>%pull(ARMA)))
    validate(need(length(arma_lst)>1, message=FALSE)) 
    
    inline = function (x) {
      tags$div(style="display:inline-block;", x)
    }
    
    # fluidRow(
    #column(8, 
    inline(
      selectizeInput(ns("which_arma"), 
                     label    = "select which dose group(s)", 
                     choices  = arma_lst, 
                     multiple = TRUE,
                     width="100%", 
                     selected = arma_lst
      )
    )
    #)
    # )
  })
  
   
   
   
  
  return(filtered_dataset())
}
