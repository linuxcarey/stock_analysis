library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  output$tableDT <- DT::renderDataTable(t.abt,
                                        options = list(paging=F),
                                        rownames=F, 
                                        filter = "top")
} 

ui <- fluidPage(
  DT::dataTableOutput("tableDT")
)

shinyApp(ui = ui, server = server)