setwd("~/R/stockVis")
# Load packages ----
library(shiny)
library(quantmod)
library(DT)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.
               
               Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates",
                     "Date range",
                     #  start = "2013-01-01",
                     # end = as.character(Sys.Date())),
                     
                     br(),
                     br(),
                     
                     checkboxInput("log", "Plot y axis on log scale",
                                   value = FALSE),
                     
                     checkboxInput("adjust",
                                   "Adjust prices for inflation", value = FALSE)
      ),
      
      mainPanel(plotOutput("plot"))
      ),
    # table output
    sidebarLayout(tableOutput("tableDT"))
  )
  # Server logic
  server <- function(input, output) {
    
    dataInput <- reactive({
      getSymbols(input$symb, src = "yahoo",
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })
    
    output$plot <- renderPlot({
      
      # chartSeries(dataInput(), theme = chartTheme("white"),
      #         type = "line", log.scale = input$log, TA = NULL)
      chartSeries(stockEnv[[stock]], theme="white", name=stock,
                  TA="addVo(); addSMA(50, col='black')", subset='last 365 days')
      
    })
    ## key statistics test 1
    
    key.statistics <- paste0("https://finance.yahoo.com/quote/", $symb, "/key-statistics?p=", $symb) %>%
      read_html() %>%
      html_table() %>%
      map_df(bind_cols) %>%
      # Transpose
      t() %>%
      as_tibble()
    
    # Set first row as column names
    colnames(key.statistics) <- key.statistics[1,]
    # Remove first row
    key.statistics <- key.statistics[-1,]
    # Add Stock Ticker as a column
    key.statistics$'Stock Ticker' <- $symb
	
      output$tableDT <- renderTable(key.statistics[6])
  }
  
  # Run the app
  shinyApp(ui, server)
  