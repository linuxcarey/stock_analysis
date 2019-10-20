library(shiny)
library(ggplot2)
library(DT)

t.abt <- read.csv("t.abt.csv")

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