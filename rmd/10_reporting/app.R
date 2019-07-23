#shinyApp(
ui <- fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Build a regression model of mpg against:',
                  choices = names(mtcars)[-1]),
      radioButtons('format', 'Document format', c( 'Word', 'PDF', 'HTML'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    mainPanel(
      plotOutput('regPlot')
    )
  )
) 


server <- function(input, output) {
  
  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })
  
  output$regPlot <- renderPlot({
    par(mar = c(4, 4, .1, .1))
    plot(regFormula(), data = mtcars, pch = 19)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report_template_pk_test.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report_template.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      
      #out <- render_report("report_template.Rmd", input, regFormula, 'sf', 134)
      out <- rmarkdown::render(
        input = 'report_template.Rmd', 
        params = list(
          region = "Europe",
          year = 2019
        ),
        output_format = switch(
         input$format,
         PDF = pdf_document(), HTML = html_document(), Word = word_document()
       ))
      
      file.rename(out, file)
    }
  )
  
} 

library(rmarkdown)
 
library(shiny)
# Run the application 
setwd("~/handbook/rmd/10_reporting")

shinyApp(ui = ui, server = server)


 

render_report = function(template, input, regFormula, region, year) {
  rmarkdown::render(
    template, params = list(
      region = region,
      year = year
    ),
    output_file = paste0("Report-", region, "-", year, ".docx")
  )
}






