
abc <- function() {
#------------------------------------
#  on UI side
#------------------------------------
output$DatasetSelector <- renderUI({
  
  selectizeInput('mDataSet', 
                 label    = "select a dataset" , 
                 choices  =  c("rock", "pressure", "cars"), 
                 multiple = FALSE, 
                 selected = "cars") 
})

output$MinNrOfEntries <- renderUI({
  
  mRange <- range(datasetInput()[,input$FlexColumn])
  mMid   <- floor(mean(mRange))
  
  sliderInput("MinNr", paste("Highlight records in", input$FlexColumn, "larger than"), 
              min = floor(mRange[1]), max = ceiling(mRange[2]), value = mMid, step = 1, ticks = FALSE)
})

output$ColumnSelector <- renderUI({
  
  selectizeInput('FlexColumn', 
                 label    = "select a column" , 
                 choices  = names(datasetInput()), 
                 multiple = FALSE) 
})


output$ColumnAlignment <- renderUI({
  
  selectizeInput('mTextAlign', 
                 label    = "text alignment" , 
                 choices  = c("left","center","right","justify"), 
                 multiple = FALSE, 
                 selected = "left") 
})

output$TextSize <- renderUI({
  
  sliderInput("mTextSize", "text size", 
              min = 1, max = 50, value = 15, step = 1, ticks = TRUE)
})

output$HeaderColor <- renderUI({
  
  selectizeInput('mHeaderColor', 
                 label    = "header color" , 
                 choices  = colors(), 
                 multiple = FALSE, 
                 selected = "limegreen") 
})

output$HighlightColor <- renderUI({
  
  selectizeInput('mHighlightColor', 
                 label    = "highlight color" , 
                 choices  = colors(), 
                 multiple = FALSE, 
                 selected = "purple2") 
})

output$BorderColor <- renderUI({
  
  selectizeInput('mBorderColor', 
                 label    = "border color" , 
                 choices  = colors(), 
                 multiple = FALSE, 
                 selected = "royalblue") 
})


output$FlexTableExample <- renderUI({
  
  # extract HTML of the FlexTable
  x <- as.html(myFlexTable())
  
  #return result in div with class "shiny-html-output"
  return(div(HTML(x), class = "shiny-html-output"))
  
})



#------------------------------------
#  on server side for ordering tables
#------------------------------------
#some data sets to choose from
datasetInput <- reactive({
  switch(input$mDataSet,
         "rock" = rock,
         "pressure" = pressure,
         "cars" = mtcars)
})


#construct a reative FlexTable object
myFlexTable <- reactive({
  
  # set text size
  options( "ReporteRs-fontsize" = input$mTextSize )
  
  # construct a FlexTable with data.frame mtcars, display rownames
  myFlexTable = FlexTable(   data = datasetInput()
                             , header.cell.props=cellProperties(background.color= input$mHeaderColor)
                             , header.text.props=textBold( color="white")
                             , add.rownames=TRUE
                             , body.par.props = parProperties(text.align = input$mTextAlign)                     
  )
  
  
  # give table zebra striped look 
  myFlexTable = setZebraStyle( myFlexTable, odd = "#CCCCCC", even = "#F5F5F5" )
  
  # add border
  whiteBorder = borderProperties( color = input$mBorderColor )
  myFlexTable = setFlexTableBorders( myFlexTable
                                     , inner.vertical = whiteBorder, inner.horizontal = whiteBorder
                                     , outer.vertical = whiteBorder, outer.horizontal = whiteBorder
  )
  
  # mark cells above a threshold red
  Mark <- datasetInput()[,input$FlexColumn] > input$MinNr
  myFlexTable[Mark,input$FlexColumn] = cellProperties(background.color= input$mHighlightColor)
  
  # add footer
  myFlexTable = addFooterRow( myFlexTable, value = "constructed with Shiny and ReporteRs"
                              , colspan = ncol(datasetInput())+1, text.properties = textItalic()
                              , par.properties = parProperties(text.alig = "center")
                              
  )
  
  return(myFlexTable)
  
})



# maintain a list of FlexTable object such that we can print and modify them later
FlexTableLog <- reactiveValues(
  mFlexTables    = list(),
  NrOfFlexTables = 0
)

#add FlexTable to log when action button is pressed
observe({
  
  if(input$AddToWordReport == 0) return()
  CurrentLog   <- isolate(FlexTableLog$mFlexTables)
  NewFlexTable <- list(isolate(myFlexTable()))
  
  FlexTableLog$mFlexTables    <- c(CurrentLog,NewFlexTable)
  FlexTableLog$NrOfFlexTables <- isolate(FlexTableLog$NrOfFlexTables) + 1
  
})

#display number of tables stored
output$TableCount <- renderText(
  FlexTableLog$NrOfFlexTables
)


}