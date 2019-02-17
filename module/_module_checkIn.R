
    ########################################################### 
    ########################################################### 
    # output$CheckinUI
    ########################################################### 
    ########################################################### 
    
    # feng's dataset
    #USUBJID, ARMA, NTIM, TIME, TEST, DVOR,LLOQ
    # 
    # in deli's typical PK dataset
    # [1] "STUDYID"           "SUBJECT"           "VISIT"             "TIMEPT"            "NOM_HR"           
    # [6] "PKHOUR_C"          "NOM_DAY"           "PKDAY_C"           "CONCCH"            "CONCN"            
    # [11] "CONCNI"            "STDUNIT"           "BLQ"               "SAMDTTM"           "ISAMDTTM"         
    # [16] "SAMDT"             "SAMTM"             "DRUGDTTM"          "DRUGDT"            "DRUGTM"           
    # [21] "IDRDTTM"           "DOSEDTTM"          "DOSESTDT"          "DOSEENDT"          "DOSE"             
    # [26] "DOSEKG"            "DOSEPLAN"          "DOSEPLANKG"        "TEST"              "TESTCD"           
    # [31] "SCAT"              "SOP"               "DIFFC"             "DIFF"              "LLOQ"             
    # [36] "SUBJECTN"          "DOSECH"            "DOSESUBJ"          "ROUTE"             "PCDY"             
    # [41] "WT"                "BSA"               "DURATION"          "TESTN"             "USUBJID"          
    # [46] "VISITNUM"          "TRT"               "COHORT"            "TRTN"              "TRTSUBJ"          
    # [51] "TRTDOSESUBJ"       "DOSEPLAN_LIM"      "PCORRES"           "COMMENT"           "CYCLE"            
    # [56] "VISITDY"           "HT"                "SEX"               "BMI"               "ETHNIC"           
    # [61] "RACE"              "AGE"               "DINT"              "DINTN"             "FIRSTE"           
    # [66] "PKDAY_C2"          "NOM_DAY2"          "GRPSUBJ"           "GRP"               "COUNTRY"          
    # [71] "QCQA2"             "QCQA1"             "VERSION2"          "VERSION1"          "GRPN"             
    # [76] "RFENDT"            "RFENTM"            "BASELINE_V3_DAY_1"
    # 

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
     
checkInUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tabsetPanel(id="which_is_which", type = "tabs", 
              
              tabPanel(title ="adpx", value="adpx", icon = icon("dataset"),
                       fluidRow(
                         fluidRow(     
                           column(width = 12, rHandsontableOutput(ns("adpx_checkin")))),
                         fluidRow(column(width = 2,
                           actionButton(
                             inputId= (ns("checkIn_adpx")),
                             label = "checkIn_adpx",
                             class = "btn-primary"   # http://getbootstrap.com/docs/4.0/components/buttons/
                           ))), 
                         
                         fluidRow(   
                           column(width = 12, uiOutput(ns("adpx_container"))))
                       )),  
              
              
              tabPanel(title ="adsl", value="adsl", icon = icon("dataset"),
                fluidRow(
                  fluidRow(     
                    column(width = 12, rHandsontableOutput(ns("adsl_checkin")))),
                  fluidRow(   
                    column(width = 12, uiOutput(ns("adsl_container"))))
                )),  
              
              
              tabPanel(title ="adex", value="adex", icon = icon("dataset"),
                       fluidRow(
                         fluidRow(     
                           column(width = 12, rHandsontableOutput(ns("adex_checkin")))),
                         fluidRow(   
                           column(width = 12, uiOutput(ns("adex_container"))))
                       )),              
              
              tabPanel(title ="adpc", value="adpc", icon = icon("dataset"),
                       fluidRow(
                         fluidRow(     
                           column(width = 12, rHandsontableOutput(ns("adpc_checkin")))),
                         fluidRow(   
                           column(width = 12, uiOutput(ns("adpc_container"))))
                       )), 
              
              tabPanel(title ="adpd", value="adpd", icon = icon("dataset"),
                       fluidRow(
                         fluidRow(     
                           column(width = 12, rHandsontableOutput(ns("adpd_checkin")))),
                         fluidRow(   
                           column(width = 12, uiOutput(ns("adpd_container"))))
                       ))              
              
              
  )
  
  
}
 
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function
checkIn <- function(input, output, session, rawData, DATA) {
    
  ns <- session$ns
  setBookmarkExclude(c("table_search"))
  
  values <- reactiveValues(adsl_checkin=NULL, adsl=NULL, 
                           adex_checkin=NULL, adex=NULL, 
                           adpc_checkin=NULL, adpc=NULL, 
                           adpd_checkin=NULL, adpd=NULL)
   
  
  #####################################################
  # adsl
  #####################################################  
  
  #Load adsl_checkin
  #----------------------
  adsl_checkin <- reactive({
    adsl = rawData$adsl()
    if (is.null(adsl)) return(NULL)    
    
    file="./lib/pkmeta.xlsx"
    adsl_checkin = read_excel(file, sheet = "adsl", col_names = TRUE) %>% as.data.frame()  
    adsl_checkin[is.na(adsl_checkin)] = ""
    adsl_checkin$Type = as.factor(adsl_checkin$Type)
    adsl_checkin= adsl_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adsl"))
    
    # fill entries with best guess
    adsl_checkin$Which_Column = as.character(adsl_checkin$Which_Column)
    
    adsl_checkin[which(adsl_checkin$Std_Name=="STUDYID"), "Which_Column"] =ifelse("STUDYID"%in%colnames(adsl), "STUDYID", "")
    
    adsl_checkin[which(adsl_checkin$Std_Name=="USUBJID"), "Which_Column"] =ifelse("USUBJID"%in%colnames(adsl), "USUBJID", 
                                                                              ifelse("SUBJID"%in%colnames(adsl), "SUBJID", 
                                                                                     ifelse("CLID"%in%colnames(adsl), "CLID","")))
    
    adsl_checkin[which(adsl_checkin$Std_Name=="ARMA"), "Which_Column"] = ifelse("ARMA"%in%colnames(adsl), "ARMA", 
                                                                                ifelse("TRTA"%in%colnames(adsl), "TRTA", 
                                                                                       ifelse("TRT01A"%in%colnames(adsl), "TRT01A", 
                                                                                              ifelse("TRTGROUP"%in%colnames(adsl), "TRTGROUP",""))))
    
    adsl_checkin[which(adsl_checkin$Std_Name=="WEIGHTBL"), "Which_Column"] = ifelse("WEIGHTBL"%in%colnames(adsl), "WEIGHTBL", 
                                                                                ifelse("WGTBL"%in%colnames(adsl), "WGTBL", 
                                                                                       ifelse("WT"%in%colnames(adsl), "WT", 
                                                                                              ifelse("WGT"%in%colnames(adsl), "WGT",""))))
    
    adsl_checkin[which(adsl_checkin$Std_Name=="HEIGHTBL"), "Which_Column"] =ifelse("HEIGHTBL"%in%colnames(adsl), "HEIGHTBL", 
                                                                               ifelse("HGTBL"%in%colnames(adsl), "HGTBL", 
                                                                                      ifelse("HT"%in%colnames(adsl), "HT", 
                                                                                             ifelse("HGT"%in%colnames(adsl), "HGT",""))))
    
    adsl_checkin[which(adsl_checkin$Std_Name=="AGE"), "Which_Column"] =ifelse("AGE"%in%colnames(adsl), "AGE", "") 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="BMIBL"), "Which_Column"] =ifelse("BMIBL"%in%colnames(adsl), "BMIBL", 
                                                                            ifelse("BMI"%in%colnames(adsl), "BMI","")) 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="BSABL"), "Which_Column"] =ifelse("BSABL"%in%colnames(adsl), "BSABL", 
                                                                            ifelse("BSA"%in%colnames(adsl), "BSA","")) 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="SEX"), "Which_Column"] =ifelse("SEX"%in%colnames(adsl), "SEX", 
                                                                          ifelse("GENDER"%in%colnames(adsl), "GENDER","")) 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="ETHNIC"), "Which_Column"] =ifelse("ETHNIC"%in%colnames(adsl), "ETHNIC", "") 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="RACE"), "Which_Column"] =ifelse("RACE"%in%colnames(adsl), "RACE", "")
    
    adsl_checkin[which(adsl_checkin$Std_Name=="COUNTRY"), "Which_Column"] =ifelse("COUNTRY"%in%colnames(adsl), "COUNTRY", "") 
    
    adsl_checkin[which(adsl_checkin$Std_Name=="SITEID"), "Which_Column"] =ifelse("SITEID"%in%colnames(adsl), "SITEID", "")     
    
    adsl_checkin
  })
  
  # display the renderRHandsontable
  output$adsl_checkin <- renderRHandsontable({
    
    adsl <- rawData$adsl()
    if (is.null(adsl)) return(NULL)
    
    # load in with default Which_Column
    adsl_checkin = adsl_checkin()
    
    # MAKE IT FACTOR
    adsl_checkin$Which_Column = factor(adsl_checkin$Which_Column, levels=c(NA, "", colnames(adsl)))
    
    values[["adsl_checkin"]] = adsl_checkin
    rhandsontable(adsl_checkin, useTypes = TRUE, stretchH = "all")
     
    #rhandsontable(ADEX,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
    #hot_col("factor_allow", allowInvalid = TRUE)
    
  })
  
  #------------------------------- 
  # update and save adsl_checkin
  #------------------------------- 
  observe({
    
    adsl_checkin <- values$adsl_checkin  #  
    
    if (!is.null(input$adsl_checkin)) {              
      adsl_checkin = hot_to_r(input$adsl_checkin)     
    } else {
      if (is.null(values[["adsl_checkin"]]))
        adsl_checkin <- adsl_checkin
      else
        adsl_checkin <- values[["adsl_checkin"]]
    }
    values[["adsl_checkin"]] <- adsl_checkin
  })
  
  #------------------------------- 
  # update and display adsl
  #------------------------------- 
  observe({
    
    adsl_checkin <- values$adsl_checkin  # 
    adsl <- rawData$adsl()
    
    if (is.null(adsl_checkin)) return(NULL)
    if (is.null(adsl)) return(NULL)
     
    adsl_checkin$Which_Column = as.character(adsl_checkin$Which_Column) # from factor to character
    
    # all key columns must have entries
    #stopifnot(adsl_checkin %>% filter(Note=="*") %>% nrow() == 
    #            adsl_checkin %>% filter(Note=="*", Which_Column!="") %>% nrow()  
    #          )
    
    if (1==2) { 
    adsl <- adsl %>% mutate_(
      STUDYID=adsl_checkin %>% filter(Std_Name=="STUDYID") %>% select(Which_Column) %>% as.character(),
      USUBJID=adsl_checkin %>% filter(Std_Name=="USUBJID") %>% select(Which_Column) %>% as.character(),
      WEIGHTBL=adsl_checkin %>% filter(Std_Name=="WEIGHTBL") %>% select(Which_Column) %>% as.character()
    )
    }
    
    #secondary columns
    std.col.lst = adsl_checkin %>% filter(  Which_Column!="", Which_Column!="NA") %>% pull(Std_Name) # select(Name) %>% as.character()
    col.lst = adsl_checkin %>% filter(  Which_Column!="",  Which_Column!="NA") %>% pull(Which_Column) #
    if (length(col.lst)>0) {adsl[, std.col.lst] = adsl[, col.lst]}
    
    #if not exist, filled with "."
    adsl[, adsl_checkin[which(adsl_checkin$Which_Column==""), "Name"]] = "."
    
    #mutate the data type  (need to check)
    #adsl = convert_colType(adsl, colnames=adsl_checkin[, "Name"], types=as.character(adsl_checkin[, "Type"]))
    
    #adsl = adsl %>% select_(adsl_checkin[, "Name"])
    #adsl = adsl[, intersect(colnames(adsl), adsl_checkin[, "Name"])] %>% filter(STUDYID!="", USUBJID!="")
    
    #adsl = adsl %>% filter(STUDYID!="", USUBJID!="")
    
    # for R3918-HV-1659 only
    if ("Treatment Group" %in% colnames(adsl)) {
      if ("REGN3918" %in% adsl[, "Treatment Group"]) {
        adsl = fun.adsl.checkin(adsl)
      }
    }
  
    values[["adsl"]] <- adsl
    
    
     
    
    
    
  })
  
  
  #output$adslTab <- DT::renderDataTable(
  #  DT::datatable(data = rawData$adsl(),
  #                options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE)  
  #  ) 
 # )
  
  ## use renderUI to display table
  output$adsl_container <- renderUI({
    output$adslTab <- DT::renderDataTable(
      DT::datatable(data = values[["adsl"]],
                    options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adslTab"))
  
  })
   
  
  
  
  #####################################################
  # adex
  #####################################################  
  
  #Load adex_checkin
  #----------------------
  adex_checkin <- reactive({
    adex = rawData$adex()
    if (is.null(adex)) return(NULL)    
    
    file="./lib/pkmeta.xlsx"
    adex_checkin = read_excel(file, sheet = "adex", col_names = TRUE) %>% as.data.frame()  
    adex_checkin[is.na(adex_checkin)] = ""
    adex_checkin$Type = as.factor(adex_checkin$Type)
    adex_checkin= adex_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adex"))
    
    # fill entries with best guess
    adex_checkin$Which_Column = as.character(adex_checkin$Which_Column)
    
    adex_checkin[which(adex_checkin$Std_Name=="STUDYID"), "Which_Column"] =ifelse("STUDYID"%in%colnames(adex), "STUDYID", "")
    
    adex_checkin[which(adex_checkin$Std_Name=="USUBJID"), "Which_Column"] =ifelse("USUBJID"%in%colnames(adex), "USUBJID", 
                                                                              ifelse("SUBJID"%in%colnames(adex), "SUBJID", 
                                                                                     ifelse("CLID"%in%colnames(adex), "CLID","")))
    
    adex_checkin[which(adex_checkin$Std_Name=="ARMA"), "Which_Column"] = ifelse("ARMA"%in%colnames(adex), "ARMA", 
                                                                                ifelse("TRTA"%in%colnames(adex), "TRTA", 
                                                                                       ifelse("TRT01A"%in%colnames(adex), "TRT01A", 
                                                                                              ifelse("TRTGROUP"%in%colnames(adex), "TRTGROUP",""))))
    
    adex_checkin[which(adex_checkin$Std_Name=="VISIT"), "Which_Column"] =ifelse("VISIT"%in%colnames(adex), "VISIT", "") 
    
    adex_checkin[which(adex_checkin$Std_Name=="TIMEPT"), "Which_Column"] =ifelse("TIMEPT"%in%colnames(adex), "TIMEPT", "") 
    
    adex_checkin[which(adex_checkin$Std_Name=="NTIM"), "Which_Column"] =ifelse("NTIM"%in%colnames(adex), "NTIM", 
                                                                            ifelse("NOM_DAY"%in%colnames(adex), "NOM_DAY","")) 
    
    adex_checkin[which(adex_checkin$Std_Name=="TIME"), "Which_Column"] =ifelse("TIME"%in%colnames(adex), "TIME", 
                                                                            ifelse("PKDAY_C"%in%colnames(adex), "PKDAY_C","")) 
    
    adex_checkin[which(adex_checkin$Std_Name=="INFHR"), "Which_Column"] =ifelse("INFHR"%in%colnames(adex), "INFHR", "")
    
    adex_checkin[which(adex_checkin$Std_Name=="EXTRT"), "Which_Column"] =ifelse("EXTRT"%in%colnames(adex), "EXTRT", "") 
    
    adex_checkin[which(adex_checkin$Std_Name=="EXROUTE"), "Which_Column"] =ifelse("EXROUTE"%in%colnames(adex), "EXROUTE", 
                                                                              ifelse("ROUTE"%in%colnames(adex), "ROUTE", ""))
    
    adex_checkin[which(adex_checkin$Std_Name=="EXDOSE"), "Which_Column"] =ifelse("EXDOSE"%in%colnames(adex), "EXDOSE", 
                                                                             ifelse("DOSE"%in%colnames(adex), "DOSE","")) 
    
    adex_checkin[which(adex_checkin$Std_Name=="EXDOSU"), "Which_Column"] =ifelse("EXDOSU"%in%colnames(adex), "EXDOSU", 
                                                                             ifelse("DOSU"%in%colnames(adex), "DOSU", ""))
    
    adex_checkin[which(adex_checkin$Std_Name=="EXSTDTC"), "Which_Column"] =ifelse("EXSTDTC"%in%colnames(adex), "EXSTDTC", "")
    
    adex_checkin[which(adex_checkin$Std_Name=="EXENDTC"), "Which_Column"] =ifelse("EXENDTC"%in%colnames(adex), "EXENDTC", "") 
    
    adex_checkin[which(adex_checkin$Std_Name=="TRTSTDTM"), "Which_Column"] =ifelse("TRTSTDTM"%in%colnames(adex), "TRTSTDTM", 
                                                                               ifelse("TRTSDTM"%in%colnames(adex), "TRTSDTM",""))
    
    adex_checkin[which(adex_checkin$Std_Name=="TRTENDTM"), "Which_Column"] =ifelse("TRTENDTM"%in%colnames(adex), "TRTENDTM", 
                                                                               ifelse("TRTEDTM"%in%colnames(adex), "TRTEDTM","")) 
    
    adex_checkin
  })
  
  # display the renderRHandsontable
  output$adex_checkin <- renderRHandsontable({
    
    adex = rawData$adex()
    if (is.null(adex)) return(NULL)  
    
    # load in with default Which_Column
    adex_checkin = adex_checkin()
    
    # MAKE IT FACTOR
    adex_checkin$Which_Column = factor(adex_checkin$Which_Column, levels=c(NA, "", colnames(adex)))
    
    values[["adex_checkin"]] = adex_checkin
    rhandsontable(adex_checkin, useTypes = TRUE, stretchH = "all")
    
    #rhandsontable(ADEX,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
    #hot_col("factor_allow", allowInvalid = TRUE)
    
  })
  
  #------------------------------- 
  # update and save adex_checkin
  #------------------------------- 
  observe({
    
    adex_checkin <- values$adex_checkin  #  
    
    if (!is.null(input$adex_checkin)) {              
      adex_checkin = hot_to_r(input$adex_checkin)     
    } else {
      if (is.null(values[["adex_checkin"]]))
        adex_checkin <- adex_checkin
      else
        adex_checkin <- values[["adex_checkin"]]
    }
    values[["adex_checkin"]] <- adex_checkin
  })
  
  #------------------------------- 
  # update and display adex
  #------------------------------- 
  observe({
    
    adex_checkin <- values$adex_checkin  # 
    adex <- rawData$adex()
    
    if (is.null(adex_checkin)) return(NULL)
    if (is.null(adex)) return(NULL)
    
    adex_checkin$Which_Column = as.character(adex_checkin$Which_Column) # from factor to character
    
    
    if (1==2) {
    adex <- adex %>% mutate_(
      STUDYID=adex_checkin %>% filter(Std_Name=="STUDYID") %>% select(Which_Column) %>% as.character(),
      USUBJID=adex_checkin %>% filter(Std_Name=="USUBJID") %>% select(Which_Column) %>% as.character(),
      ARMA=adex_checkin %>% filter(Std_Name=="ARMA") %>% select(Which_Column) %>% as.character(),
      TIMEPT=adex_checkin %>% filter(Std_Name=="TIMEPT") %>% select(Which_Column) %>% as.character(),
      EXTRT=adex_checkin %>% filter(Std_Name=="EXTRT") %>% select(Which_Column) %>% as.character(),
      EXROUTE=adex_checkin %>% filter(Std_Name=="EXROUTE") %>% select(Which_Column) %>% as.character(),
      EXDOSE=adex_checkin %>% filter(Std_Name=="EXDOSE") %>% select(Which_Column) %>% as.character(),
      EXDOSU=adex_checkin %>% filter(Std_Name=="EXDOSU") %>% select(Which_Column) %>% as.character(),
      EXSTDTC=adex_checkin %>% filter(Std_Name=="EXSTDTC") %>% select(Which_Column) %>% as.character(),
      EXENDTC=adex_checkin %>% filter(Std_Name=="EXENDTC") %>% select(Which_Column) %>% as.character(),
      TRTSTDTM=adex_checkin %>% filter(Std_Name=="TRTSTDTM") %>% select(Which_Column) %>% as.character()
    )
    }
    
    #secondary columns
    std.col.lst = adex_checkin %>% filter(  Which_Column!="", Which_Column!="NA") %>% pull(Std_Name) # select(Name) %>% as.character()
    col.lst = adex_checkin %>% filter( Which_Column!="",  Which_Column!="NA") %>% pull(Which_Column) #
    ids = which(col.lst %in% colnames(adex))
    if (length(col.lst[ids])>0) {adex[, std.col.lst[ids]] = adex[, col.lst[ids]]}
    
    #if not exist, filled with "."
    adex[, adex_checkin[which(adex_checkin$Which_Column==""), "Name"]] = "."
    
    #mutate the data type  (need to check)
    #adex = convert_colType(adex, colnames=adex_checkin[, "Name"], types=as.character(adex_checkin[, "Type"]))
    
    #adex = adex %>% select_(adex_checkin[, "Name"])
    #adex = adex[, intersect(colnames(adex), adex_checkin[, "Name"])] %>% filter(STUDYID!="", USUBJID!="")
    
    values[["adex"]] <- adex
    
  })
  
  
  ## use renderUI to display table
  output$adex_container <- renderUI({
    output$adexTab <- DT::renderDataTable(
      DT::datatable(data = values[["adex"]],
                    options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adexTab"))
    
  })
  
  
  
  #####################################################
  # adpc
  #####################################################  
  
  #Load adpc_checkin
  #----------------------
  adpc_checkin <- reactive({
    adpc = rawData$adpc()
    if (is.null(adpc)) return(NULL)  
    
    file="./lib/pkmeta.xlsx"
    adpc_checkin = read_excel(file, sheet = "adpc", col_names = TRUE) %>% as.data.frame()  
    adpc_checkin[is.na(adpc_checkin)] = ""
    adpc_checkin$Type = as.factor(adpc_checkin$Type)
    adpc_checkin= adpc_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adpc"))
    
    # fill entries with best guess
    adpc_checkin$Which_Column = as.character(adpc_checkin$Which_Column)
    adpc_checkin$Std_Name = as.character(adpc_checkin$Std_Name)    
    
    adpc_checkin[which(adpc_checkin$Std_Name=="STUDYID"), "Which_Column"] =ifelse("STUDYID"%in%colnames(adpc), "STUDYID", "")
    
    adpc_checkin[which(adpc_checkin$Std_Name=="USUBJID"), "Which_Column"] =ifelse("USUBJID"%in%colnames(adpc), "USUBJID", 
                                                                              ifelse("SUBJID"%in%colnames(adpc), "SUBJID", 
                                                                                     ifelse("CLID"%in%colnames(adpc), "CLID","")))
    
    adpc_checkin[which(adpc_checkin$Std_Name=="ARMA"), "Which_Column"] = ifelse("ARMA"%in%colnames(adpc), "ARMA", 
                                                                                ifelse("TRTA"%in%colnames(adpc), "TRTA", 
                                                                                       ifelse("TRT01A"%in%colnames(adpc), "TRT01A", 
                                                                                              ifelse("TRTGROUP"%in%colnames(adpc), "TRTGROUP",""))))
    
    adpc_checkin[which(adpc_checkin$Std_Name=="VISIT"), "Which_Column"] =ifelse("VISIT"%in%colnames(adpc), "VISIT", "") 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="TIMEPT"), "Which_Column"] =ifelse("TIMEPT"%in%colnames(adpc), "TIMEPT", "") 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="NTIM"), "Which_Column"] =ifelse("NTIM"%in%colnames(adpc), "NTIM", 
                                                                           ifelse("NOM_DAY"%in%colnames(adpc), "NOM_DAY","")) 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="TIME"), "Which_Column"] =ifelse("TIME"%in%colnames(adpc), "TIME", 
                                                                           ifelse("PKDAY_C"%in%colnames(adpc), "PKDAY_C","")) 
     
    adpc_checkin[which(adpc_checkin$Std_Name=="SAMDTTM"), "Which_Column"] =ifelse("SAMDTTM"%in%colnames(adpc), "SAMDTTM", "")
    
    adpc_checkin[which(adpc_checkin$Std_Name=="SOP"), "Which_Column"] =ifelse("SOP"%in%colnames(adpc), "SOP", "") 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="TEST"), "Which_Column"] =ifelse("TEST"%in%colnames(adpc), "TEST", "") 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="DVOR"), "Which_Column"] =ifelse("DVOR"%in%colnames(adpc), "DVOR", 
                                                                               ifelse("RESC_RAW"%in%colnames(adpc), "RESC_RAW",
                                                                                      ifelse("RESC"%in%colnames(adpc), "RESC",""))) 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="STDUNIT"), "Which_Column"] =ifelse("STDUNIT"%in%colnames(adpc), "STDUNIT", 
                                                                              ifelse("UNIT"%in%colnames(adpc), "UNIT", "")) 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="BLQ"), "Which_Column"] =ifelse("BLQ"%in%colnames(adpc), "BLQ", "") 
    
    adpc_checkin[which(adpc_checkin$Std_Name=="LLOQ"), "Which_Column"] =ifelse("LLOQ"%in%colnames(adpc), "LLOQ", "")     
    
    adpc_checkin
  })
  
  # display the renderRHandsontable
  output$adpc_checkin <- renderRHandsontable({
    
    adpc = rawData$adpc()
    if (is.null(adpc)) return(NULL)  
    
    # load in with default Which_Column
    adpc_checkin = adpc_checkin()
    
    # MAKE IT FACTOR
    adpc_checkin$Which_Column = factor(adpc_checkin$Which_Column, levels=c(NA, "", colnames(adpc)))
    
    values[["adpc_checkin"]] = adpc_checkin
    rhandsontable(adpc_checkin, useTypes = TRUE, stretchH = "all")
    
    #rhandsontable(adpc,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
    #hot_col("factor_allow", allowInvalid = TRUE)
    
  })
  
  #------------------------------- 
  # update and save adpc_checkin
  #------------------------------- 
  observe({
    
    adpc_checkin <- values$adpc_checkin  #  
    
    if (!is.null(input$adpc_checkin)) {              
      adpc_checkin = hot_to_r(input$adpc_checkin)     
    } else {
      if (is.null(values[["adpc_checkin"]]))
        adpc_checkin <- adpc_checkin
      else
        adpc_checkin <- values[["adpc_checkin"]]
    }
    values[["adpc_checkin"]] <- adpc_checkin
  })
  
  #------------------------------- 
  # update and display adpc
  #------------------------------- 
  observe({
    
    adpc_checkin <- values$adpc_checkin  # 
    adpc <- rawData$adpc()
    
    if (is.null(adpc_checkin)) return(NULL)
    if (is.null(adpc)) return(NULL)
    
    adpc_checkin$Which_Column = as.character(adpc_checkin$Which_Column) # from factor to character
    
    # all key columns must have entries
    #stopifnot(adpc_checkin %>% filter(Note=="*"|Note=="**") %>% nrow() == 
    #            adpc_checkin %>% filter(Note=="*"|Note=="**", Which_Column!="") %>% nrow()  
   # )
    
   if (1==2) {
    adpc <- adpc %>% mutate_(
      STUDYID=adpc_checkin %>% filter(Std_Name=="STUDYID") %>% select(Which_Column) %>% as.character(),
      USUBJID=adpc_checkin %>% filter(Std_Name=="USUBJID") %>% select(Which_Column) %>% as.character(),
      TIMEPT=adpc_checkin %>% filter(Std_Name=="TIMEPT") %>% select(Which_Column) %>% as.character(),
      NTIM=adpc_checkin %>% filter(Std_Name=="NTIM") %>% select(Which_Column) %>% as.character(),
      TIME=adpc_checkin %>% filter(Std_Name=="TIME") %>% select(Which_Column) %>% as.character(),
      SAMDTTM=adpc_checkin %>% filter(Std_Name=="SAMDTTM") %>% select(Which_Column) %>% as.character(),
      SOP=adpc_checkin %>% filter(Std_Name=="SOP") %>% select(Which_Column) %>% as.character(),
      TEST=adpc_checkin %>% filter(Std_Name=="TEST") %>% select(Which_Column) %>% as.character(),
      DVOR=adpc_checkin %>% filter(Std_Name=="DVOR") %>% select(Which_Column) %>% as.character(),
      STDUNIT=adpc_checkin %>% filter(Std_Name=="STDUNIT") %>% select(Which_Column) %>% as.character(), 
      BLQ=adpc_checkin %>% filter(Std_Name=="BLQ") %>% select(Which_Column) %>% as.character(), 
      LLOQ=adpc_checkin %>% filter(Std_Name=="LLOQ") %>% select(Which_Column) %>% as.character()       
    )
   }
    
    #secondary columns
    std.col.lst = adpc_checkin %>% filter( Which_Column!="", Which_Column!="NA") %>% pull(Std_Name) # select(Name) %>% as.character()
    col.lst = adpc_checkin %>% filter( Which_Column!="",  Which_Column!="NA") %>% pull(Which_Column) #
    if (length(col.lst)>0) {adpc[, std.col.lst] = adpc[, col.lst]}
    
    #if not exist, filled with "."
    adpc[, adpc_checkin[which(adpc_checkin$Which_Column==""), "Name"]] = "."
    
    #mutate the data type  (need to check)
    #adpc = convert_colType(adpc, colnames=adpc_checkin[, "Name"], types=as.character(adpc_checkin[, "Type"]))
    
    # calculate derived NTIM, TIME, etc. if necessary
    #-------------------------------------------------
    adsl = rawData$adsl()
    adex = rawData$adex()
    
    unique(adpc$SOP)    
    SOP = c("PCL3671", "PCL3679")
    LLOQ = c(0.078, 0.0195)
    LLOQ = data.frame(SOP, LLOQ)
    LLOQ
    
    unique(adpc$TIMEPT)
    TIMEPT = c("SCREENING", "EOS", "ET")
    NTIM = c(-14, 112, 88)
    NTIM_MANUAL = data.frame(TIMEPT, NTIM)
    
    # standarlize the adpc
    adsl =NULL
    adex = NULL
    adpc = build_adpc(adpc, adsl=adsl, adex=adex, INFHR=1, CYCLE_DURATION=0, LLOQ, NTIM_MANUAL)
    
    key.col.lst = c("STUDYID", "USUBJID", "SUBJECT", "ARMA", 
                    "VISIT", "TIMEPT", "NTIM", "TIME",  
                    "SOP", "TEST", "DVOR", "CONCN",  "CONCCH", "CONCNI", "STDUNIT", "BLQ", "LLOQ", 
                    "QCQA1", "QCQA2")
    col.lst = c(key.col.lst, setdiff(colnames(adpc), key.col.lst))
    adpc = adpc[, col.lst]
    
    #print(head(adpc, n= 25) %>% as.data.frame())
    
    # output adpc
    #adpc = adpc %>% select_(adpc_checkin[, "Name"])
    #adpc = adpc[, intersect(colnames(adpc), adpc_checkin[, "Name"])] %>% filter(STUDYID!="", USUBJID!="")
    
    values[["adpc"]] <- adpc
    
  })
  
  
  ## use renderUI to display table
  output$adpc_container <- renderUI({
    output$adpcTab <- DT::renderDataTable(
      DT::datatable(data = values[["adpc"]],
                    options = list(pageLength = 3, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adpcTab"))
    
  })
  
  
  
  #####################################################
  # adpd
  #####################################################  
  
  #Load adpd_checkin
  #----------------------
  adpd_checkin <- reactive({
    adpd = rawData$adpd()
    if (is.null(adpd)) return(NULL)    
    
    file="./lib/pkmeta.xlsx"
    adpd_checkin = read_excel(file, sheet = "adpd", col_names = TRUE) %>% as.data.frame()  
    adpd_checkin[is.na(adpd_checkin)] = ""
    adpd_checkin$Type = as.factor(adpd_checkin$Type)
    adpd_checkin= adpd_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adpd"))
    
    # fill entries with best guess
    adpd_checkin$Which_Column = as.character(adpd_checkin$Which_Column)
    
    adpd_checkin[which(adpd_checkin$Std_Name=="STUDYID"), "Which_Column"] =ifelse("STUDYID"%in%colnames(adpd), "STUDYID", "")
    
    adpd_checkin[which(adpd_checkin$Std_Name=="USUBJID"), "Which_Column"] =ifelse("USUBJID"%in%colnames(adpd), "USUBJID", 
                                                                              ifelse("SUBJID"%in%colnames(adpd), "SUBJID", 
                                                                                     ifelse("CLID"%in%colnames(adpd), "CLID","")))
    
    adpd_checkin[which(adpd_checkin$Std_Name=="ARMA"), "Which_Column"] = ifelse("ARMA"%in%colnames(adpd), "ARMA", 
                                                                                ifelse("TRTA"%in%colnames(adpd), "TRTA", 
                                                                                       ifelse("TRT01A"%in%colnames(adpd), "TRT01A", 
                                                                                              ifelse("TRTGROUP"%in%colnames(adpd), "TRTGROUP",""))))
    
    adpd_checkin[which(adpd_checkin$Std_Name=="VISIT"), "Which_Column"] =ifelse("VISIT"%in%colnames(adpd), "VISIT", "") 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="TIMEPT"), "Which_Column"] =ifelse("TIMEPT"%in%colnames(adpd), "TIMEPT", "") 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="NTIM"), "Which_Column"] =ifelse("NTIM"%in%colnames(adpd), "NTIM", 
                                                                           ifelse("NOM_DAY"%in%colnames(adpd), "NOM_DAY","")) 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="TIME"), "Which_Column"] =ifelse("TIME"%in%colnames(adpd), "TIME", 
                                                                           ifelse("PKDAY_C"%in%colnames(adpd), "PKDAY_C","")) 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="SAMDTTM"), "Which_Column"] =ifelse("SAMDTTM"%in%colnames(adpd), "SAMDTTM", "")
    
    adpd_checkin[which(adpd_checkin$Std_Name=="SOP"), "Which_Column"] =ifelse("SOP"%in%colnames(adpd), "SOP", "") 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="TEST"), "Which_Column"] =ifelse("TEST"%in%colnames(adpd), "TEST", "") 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="DVOR"), "Which_Column"] =ifelse("DVOR"%in%colnames(adpd), "DVOR", 
                                                                           ifelse("RESC_RAW"%in%colnames(adpd), "RESC_RAW",
                                                                                  ifelse("RESC"%in%colnames(adpd), "RESC",""))) 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="STDUNIT"), "Which_Column"] =ifelse("STDUNIT"%in%colnames(adpd), "STDUNIT", 
                                                                              ifelse("UNIT"%in%colnames(adpd), "UNIT", "")) 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="BLQ"), "Which_Column"] =ifelse("BLQ"%in%colnames(adpd), "BLQ", "") 
    
    adpd_checkin[which(adpd_checkin$Std_Name=="LLOQ"), "Which_Column"] =ifelse("LLOQ"%in%colnames(adpd), "LLOQ", "")     
    
    adpd_checkin
  })
  
  # display the renderRHandsontable
  output$adpd_checkin <- renderRHandsontable({
    
    adpd = rawData$adpd()
    if (is.null(adpd)) return(NULL)  
    
    # load in with default Which_Column
    adpd_checkin = adpd_checkin()
    
    # MAKE IT FACTOR
    adpd_checkin$Which_Column = factor(adpd_checkin$Which_Column, levels=c(NA, "", colnames(adpd)))
    
    values[["adpd_checkin"]] = adpd_checkin
    rhandsontable(adpd_checkin, useTypes = TRUE, stretchH = "all")
    
    #rhandsontable(adpd,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
    #hot_col("factor_allow", allowInvalid = TRUE)
    
  })
  
  #------------------------------- 
  # update and save adpd_checkin
  #------------------------------- 
  observe({
    
    adpd_checkin <- values$adpd_checkin  #  
    
    if (!is.null(input$adpd_checkin)) {              
      adpd_checkin = hot_to_r(input$adpd_checkin)     
    } else {
      if (is.null(values[["adpd_checkin"]]))
        adpd_checkin <- adpd_checkin
      else
        adpd_checkin <- values[["adpd_checkin"]]
    }
    values[["adpd_checkin"]] <- adpd_checkin
  })
  
  #------------------------------- 
  # update and display adpd
  #------------------------------- 
  observe({
    
    adpd_checkin <- values$adpd_checkin  # 
    adpd <- rawData$adpd()
    
    if (is.null(adpd_checkin)) return(NULL)
    if (is.null(adpd)) return(NULL)
    
    adpd_checkin$Which_Column = as.character(adpd_checkin$Which_Column) # from factor to character
    
    # all key columns must have entries
    #stopifnot(adpd_checkin %>% filter(Note=="*"|Note=="**") %>% nrow() == 
    #            adpd_checkin %>% filter(Note=="*"|Note=="**", Which_Column!="") %>% nrow()  
    # )
    
    if (1==2) {
      adpd <- adpd %>% mutate_(
        STUDYID=adpd_checkin %>% filter(Std_Name=="STUDYID") %>% select(Which_Column) %>% as.character(),
        USUBJID=adpd_checkin %>% filter(Std_Name=="USUBJID") %>% select(Which_Column) %>% as.character(),
        TIMEPT=adpd_checkin %>% filter(Std_Name=="TIMEPT") %>% select(Which_Column) %>% as.character(),
        NTIM=adpd_checkin %>% filter(Std_Name=="NTIM") %>% select(Which_Column) %>% as.character(),
        TIME=adpd_checkin %>% filter(Std_Name=="TIME") %>% select(Which_Column) %>% as.character(),
        SAMDTTM=adpd_checkin %>% filter(Std_Name=="SAMDTTM") %>% select(Which_Column) %>% as.character(),
        SOP=adpd_checkin %>% filter(Std_Name=="SOP") %>% select(Which_Column) %>% as.character(),
        TEST=adpd_checkin %>% filter(Std_Name=="TEST") %>% select(Which_Column) %>% as.character(),
        DVOR=adpd_checkin %>% filter(Std_Name=="DVOR") %>% select(Which_Column) %>% as.character(),
        STDUNIT=adpd_checkin %>% filter(Std_Name=="STDUNIT") %>% select(Which_Column) %>% as.character(), 
        BLQ=adpd_checkin %>% filter(Std_Name=="BLQ") %>% select(Which_Column) %>% as.character(), 
        LLOQ=adpd_checkin %>% filter(Std_Name=="LLOQ") %>% select(Which_Column) %>% as.character()       
      )
    }
    
    #secondary columns
    std.col.lst = adpd_checkin %>% filter( Which_Column!="", Which_Column!="NA") %>% pull(Std_Name) # select(Name) %>% as.character()
    col.lst = adpd_checkin %>% filter( Which_Column!="",  Which_Column!="NA") %>% pull(Which_Column) #
    col.lst = intersect(col.lst, colnames(adpd))
    if (length(col.lst)>0) {adpd[, std.col.lst] = adpd[, col.lst]}
    
    #if not exist, filled with "."
    adpd[, adpd_checkin[which(adpd_checkin$Which_Column==""), "Name"]] = "."
    
    #mutate the data type  (need to check)
    #adpd = convert_colType(adpd, colnames=adpd_checkin[, "Name"], types=as.character(adpd_checkin[, "Type"]))
    
    #adpd = adpd %>% select_(adpd_checkin[, "Name"])
    #adpd = adpd %>% filter(STUDYID!="", USUBJID!="")
    
    # # for R3918-HV-1659 only
    # if ("Investigator Name" %in% colnames(adpd)) {
    #   if ("Bush, Jim" %in% adpd[, "Investigator Name"]) {
    #     adpd = fun.adpd.checkin(rawData$adpd(), 
    #                             fun.adsl.checkin(rawData$adsl()))
    #   }
    # } 
    # 
    # if ("R3918-HV-1659" %in% adpd$STUDYID) {
    #   adpd = adpd %>% select(STUDYID, ARMA, USUBJID,SUBJECT,
    #                          TEST,STDUNIT,VISIT, TIMEPT,NTIM,TIME,SAMDTTM, BASE,DVOR,CHG,PCHG)
    # }
    # 
    values[["adpd"]] <- adpd
    
  })
  
  
  ## use renderUI to display table
  output$adpd_container <- renderUI({
    output$adpdTab <- DT::renderDataTable(
      DT::datatable(data = values[["adpd"]],
                    options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adpdTab"))
    
  })

    
  
  #####################################################
  # adpx
  #####################################################  
  
  #Load adpx_checkin
  #----------------------
  default_adpx_checkin <- reactive({
    file="./lib/pkmeta.xlsx"
    adpx_checkin = read_excel(file, sheet = "adpx", col_names = TRUE) %>% as.data.frame()  
    adpx_checkin[is.na(adpx_checkin)] = ""
    adpx_checkin$Type = as.factor(adpx_checkin$Type)
    adpx_checkin= adpx_checkin %>% select(Domain:Note) %>% filter(Domain%in%c("adpx"))
    adpx_checkin
  })
   
  
 observe({  
    adpx_checkin = default_adpx_checkin()
    
    # if new adpx loaded
    adpx = rawData$adpx()
    if (is.null(adpx)) {return(adpx_checkin)}
 
    # based on adpx, update adpx_checkin, note VARS use globalVars$magicTab on background
    adpx_checkin$Which_Column = VARS(adpx, vars.lst=adpx_checkin$Std_Name, Where="indivProfile", Who="feng.yang") 
    #adpx_checkin$Which_Column = factor(adpx_checkin$Which_Column, levels=c(NA, colnames(adpx)))

    values[["adpx_checkin"]] = adpx_checkin   
  })
  
  
  
  # display the renderRHandsontable
  output$adpx_checkin <- renderRHandsontable({
   
    adpx = rawData$adpx()
    if (is.null(adpx)) return(NULL)  
    
    adpx_checkin = isolate(values[["adpx_checkin"]])
    if (is.null(adpx_checkin)) {return(NULL)}
 
    rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all")
 
  })
  
  #------------------------------- 
  # update and save adpx_checkin
  #------------------------------- 
  observe({
    
    adpx_checkin <- values$adpx_checkin  #  
    
    if (!is.null(input$adpx_checkin)) {              
      adpx_checkin = hot_to_r(input$adpx_checkin)     
    } else {
      if (is.null(values[["adpx_checkin"]]))
        adpx_checkin <- adpx_checkin
      else
        adpx_checkin <- values[["adpx_checkin"]]
    }
    values[["adpx_checkin"]] <- adpx_checkin
  })
  
  #------------------------------- 
  # update and display adpx
  #------------------------------- 
  observeEvent(input$checkIn_adpx, {
    
   
    adpx_checkin <- values$adpx_checkin  # 
    adpx <- rawData$adpx()
    
    if (is.null(adpx_checkin)) return(NULL)
    if (is.null(adpx)) return(NULL)
    
    #globalVars$magicTab

    globalVars$magicTab = push2MagicTab(globalVars$magicTab,
                          Domain=as.character(adpx_checkin$Std_Name),
                          Alias=as.character(adpx_checkin$Which_Column),
                          Label="",
                          Where="",
                          Who="feng.yang")

 # print("updated magicTab")
 # print(adpx_checkin %>% as.data.frame())
 # print(globalVars$magicTab %>% filter(Domain=="ARMA"))

    #adpx_checkin$Which_Column = as.character(adpx_checkin$Which_Column) # from factor to character
 
    #secondary columns
    tt = adpx_checkin %>% filter( Which_Column!="", !is.na(Which_Column), Which_Column!="NA")
 
    if (nrow(tt)>0) { adpx[, tt$Std_Name] = adpx[, as.character(tt$Which_Column)]}
    #adpx[, adpx_checkin[which(is.na(adpx_checkin$Which_Column)), "Std_Name"]] = "."
    
    
    # %>% pull(Std_Name) # select(Name) %>% as.character()
    # col.lst = adpx_checkin %>% filter( Which_Column!="",  !is.na(Which_Column)) %>% pull(Which_Column) #
    # col.lst = intersect(col.lst, colnames(adpx))
    # if (length(col.lst)>0) {adpx[, std.col.lst] = adpx[, col.lst]}
    # 
    # #if not exist, filled with "."
    # adpx[, adpx_checkin[which(is.na(adpx_checkin$Which_Column)), "Std_Name"]] = "."
    
    #mutate the data type  (need to check)
    #adpx = convert_colType(adpx, colnames=adpx_checkin[, "Name"], types=as.character(adpx_checkin[, "Type"]))
    
    #adpx = adpx %>% select_(adpx_checkin[, "Name"])
    #adpx = adpx %>% filter(STUDYID!="", USUBJID!="")
    
    # for R3918-HV-1659 only
    # if (1==2) {
    #   if ("Investigator Name" %in% colnames(adpx)) {
    #     if ("Bush, Jim" %in% adpx[, "Investigator Name"]) {
    #       adpx = fun.adpx.checkin(rawData$adpx(), 
    #                               fun.adsl.checkin(rawData$adsl()))
    #     }
    #   } 
    #   
    #   if ("R3918-HV-1659" %in% adpx$STUDYID) {
    #     adpx = adpx %>% select(STUDYID, ARMA, USUBJID,SUBJECT,
    #                            TEST,STDUNIT,VISIT, TIMEPT,NTIM,TIME,SAMDTTM, BASE,DVOR,CHG,PCHG)
    #   }
    # }
    
    values[["adpx"]] <- adpx
  
    
  })
  
  
  ## use renderUI to display table
  output$adpx_container <- renderUI({
    output$adpxTab <- DT::renderDataTable(
      DT::datatable(data = values[["adpx"]],
                    options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adpxTab"))
    
  })

    
  adsl<- reactive({values[["adsl"]]})
  adex<- reactive({values[["adex"]]})
  adpc<- reactive({values[["adpc"]]})
  adpd<- reactive({values[["adpd"]]})  
  
  adpx<- reactive({values[["adpx"]]})   
  
  adpx2<- reactive({
    # default
    adpx<- NULL   # for now, need to integrate adsl, adex, adpc, adpd later.
    
    # use adpc 
    if (!is.null(adpc)) {adpx = adpc }
    
    # if adpx
    if (!is.null(rawData$adpx())) {adpx = rawData$adpx() }
    
    adpx
    #values[["adpd"]]
    
    
    #DATA$adsl = adsl();
    #DATA$adex = adex(); 
    #DATA$adpc = adpc(); 
    #DATA$adpd = adpd(); 
    #DATA$adpx = adpx;
    
    adpx
    })
  
  
  
 
  observe({
 
    data_name = "adpx"
    ADPX = rawData$adpx() 
    adpx = adpx()
    class.lst = sapply(ADPX,class)
      
    isolate({ 
      # a single data.frame
      if (!"data.frame" %in% class.lst) {
        DATA$mDATA[[data_name]] = ADPX
      }
        
      if (!is.null(adpx))  {DATA$mDATA[[data_name]] = adpx}
        
      # list of more than 1 data.frame
      if ("data.frame" %in% class.lst) { 
        DATA$NumOfDataset = DATA$NumOfDataset + length(ADPX)
        for (idata_name in names(ADPX)) {
          DATA$mDATA[[idata_name]] = ADPX[[idata_name]]
          #DATA$mTITLES[[idata_name]] =  idata_name 
        }}
    })
    
  })

  
  
  # Return the reactive that yields the data frame
  return(DATA)
  
}
 