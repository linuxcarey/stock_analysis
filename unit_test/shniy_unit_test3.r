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
          tags$strong("Price/Sales ratio (P/S)")
        , tableOutput("tableDT")
        , tags$strong("Reason")
        , tableOutput("reason")
        , tags$strong("Action")
        , tableOutput("action")
        # , textOutput("intro")
        # , includeMarkdown("test.md")
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
  output$section1 <- renderText( paste("Section 1. Price/Sales ratio")
                                #%>% dplyr::rename(" " = data)
  )
  
  # output$tableDT <- renderTable(
  output$tableDT <- renderTable(
    paste0("https://www.marketwatch.com/investing/Stock/", input$symb, "/analystestimates") %>%
      read_html() %>%
      html_nodes(xpath='//td | //*[contains(concat( " ", @class, " " ), concat( " ", "divider", " " ))]') %>%
      html_text() %>%
      enframe(name = NULL) %>% 
      dplyr::rename('Number of Ratings' = value)
    %>% dplyr::slice(7)
    ,
    width = "auto", colnames = TRUE
    )
  output$reason <- renderTable(
paste("Institutional buying is an important catalyst for share price growth.
Institutions buy hundreds of thousands of shares and prefer stocks with
      large daily trading volumes so they can easily move in and out of positions.")
, colnames = FALSE
  )

  output$action <- renderTable(
paste('O.K. to buy if Average Daily Volume is 150,000 shares or higher, and above one million shares is best.')
, colnames = FALSE
)
  # output$intro <- renderText("this is intro text that I am testing here")
}

# Run the app
shinyApp(ui, server)
