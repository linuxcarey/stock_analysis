setwd("~/R/stockVis")
# Load packages ----
library(shiny)
library(quantmod)
library(DT)
library(xml2)
library(rvest)
library(tibble)


# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.
               
               Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "FNV")
    ),   
      # table output
      mainPanel(      
        tableOutput("tableDT"),
        textOutput("intro"),
        includeMarkdown("test.md")
        )
    )
  )

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               auto.assign = FALSE)
  })
  
  ## key statistics test 1
  
  
  output$tableDT <- renderTable(
  paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
    read_html() %>%
    html_table() %>%
    purrr::map_df(dplyr::bind_cols) %>%
    # Transpose
    t() %>%
    as_tibble()
  output$intro <- renderText("this is intro text that I am testing here")
  )
}

# Run the app
shinyApp(ui, server)
