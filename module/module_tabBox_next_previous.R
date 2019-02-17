# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# https://antoineguillot.wordpress.com/2017/02/15/three-r-shiny-tricks-to-make-your-shiny-app-shines-13/

#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_tabBox_next_previous_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(box(width=12,
                    tabBox(width=12,id=ns("tabBox_next_previous"),
                           tabPanel("Tab1",p("This is tab 1")),
                           tabPanel("Tab2",p("This is tab 2")),
                           tabPanel("Tab3",p("This is tab 3")),
                           tabPanel("Tab4",p("This is tab 4")),
                           tags$script("
                       $('body').mouseover(function() {
                       list_tabs=[];
                       $('#tabBox_next_previous li a').each(function(){
                       list_tabs.push($(this).html())
                       });
                       Shiny.onInputChange('List_of_tab', list_tabs);})
                       "
                           )
                    ),
                    uiOutput(ns("Next_Previous"))
  ))
)

}


module_tabBox_next_previous <- function(input, output,session) {
  ns <- session$ns
  
  Previous_Button=tags$div(actionButton(ns("Prev_Tab"),HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                      ')))
  Next_Button=div(actionButton(ns("Next_Tab"),HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))
  
  
  
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    
    validate(need(tab_list, message=FALSE), 
             need(input$tabBox_next_previous, message=FALSE))
    
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    
  })
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               }
  )
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               }
  )
  

}
 


