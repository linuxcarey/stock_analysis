setwd("~/R/stockVis")
# Load packages ----
library(shiny)
library(shinythemes)
library(quantmod)
library(DT)
library(xml2)
library(rvest)
library(tibble)
# Source helpers ----
source("helpers.R")

# User interface ----

ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = h4("Stock Analysis Checklist, as per Harry Domash's Winning Investing"),
  tabPanel("Introduction",
           sidebarLayout(
             sidebarPanel(),
             mainPanel() 
           )
  ),
  tabPanel("Basic Research - Part 1",
           sidebarLayout(
             sidebarPanel(
               helpText("Select a stock to examine. Information will be collected from Yahoo Finance."),
               textInput("symb", "Symbol", "FNV"),
               tableOutput("cmpname"),
               tableOutput("cmpdesc")
             ),
             mainPanel(
               # Basic Analysis 
               # 1
               tags$strong("1. One-year Price chart with 50-day moving average")
               , tags$p()
               , plotOutput("plot")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("cmpreason")
               , tags$strong("Action")
               , tableOutput("cmpaction")
               , tags$p()
               # 2
               , tags$strong("2. Price/Sales ratio")
               , tags$p()
               , tableOutput("stat01")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat01rsn")
               , tags$strong("Action")
               , tableOutput("stat01actn")
               , tags$p()
               # 3
               , tags$strong("3. Price to Cash Flow ratio")
               , tags$p()
               , tableOutput("stat02")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat02rsn")
               , tags$strong("Action")
               , tableOutput("stat02actn")
             )
           )
  ),
  tabPanel("Advanced Research & Analysis - Part 2",
           sidebarLayout(
             sidebarPanel(),
             mainPanel()
           )
  ),
  tabPanel("Acknowledments",
           sidebarLayout(
             sidebarPanel(),
             mainPanel()
           )
  )
)
# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput(), theme = chartTheme("white"), subset='last 365 days',
                type = "line", name =" ",
                TA = "addVo(); addSMA(50, col='black')"
    )
    
  })
  # Part 0
  # Company name
  
  output$cmpname <- renderTable(
    paste0("https://www.marketwatch.com/investing/Stock/", input$symb,"/profile") %>%
      read_html() %>%
      html_nodes(xpath='//*[(@id = "instrumentname")]') %>%
      html_text() %>%
      enframe(name = NULL) %>% 
      dplyr::rename(Company = value),
    width = "auto", colnames = TRUE
  )
  
  # Company Description
  
  output$cmpdesc <- renderTable(
    paste0("https://www.marketwatch.com/investing/Stock/", input$symb,"/profile") %>% 
      read_html() %>%
      html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "limited", " " ))]') %>%
      html_text() %>% 
      enframe(name = NULL) %>% 
      dplyr::rename(Description = value),
    width = "auto", colnames = TRUE
  )
  output$cmpreason <- renderTable( paste("Buying a stock while it’s in a downtrend is dangerous, as it will likely move lower. 
                                         A stock is in a downtrend if its price is below its moving
                                         average (MA), and in an uptrend if above. Use the 50-day Simple Moving Average (SMA).")
                                   , colnames = FALSE
  )
  
  output$cmpaction <- renderTable( paste('O.K. to buy if stock price is above its 50-day moving average.')
                                   , colnames = FALSE
  )
  ## key statistics
  # Price to Sales
  output$stat01 <- renderTable(
    paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
      read_html() %>%
      html_table() %>%
      purrr::map_df(dplyr::bind_cols) %>%
      # Transpose
      t() %>%
      as_tibble() 
    %>% dplyr::select(" " = V6)
    
  )
  output$stat01rsn <- renderTable( 
paste("Valuation check. A stock with a P/S above 10 is momentum priced.
      Buying momentum priced stocks is only recommended in a strong market.")
, colnames = FALSE
)
  
  output$stat01actn <- renderTable(
paste('O.K. to buy if P/S is less than 10. P/S ratios between 3 and 5 are best for growth stocks.
      Ratios below 2 reflect value priced stocks.')
, colnames = FALSE
)
  # Price to Cash Flow
  output$stat02 <- renderTable(
    paste0("https://www.marketwatch.com/investing/Stock/",input$symb,"/profile/") %>%
      read_html() %>%
      html_nodes(xpath='//*[@class="sixwide addgutter"]') %>%
      html_nodes(xpath='//*[@class="section"]') %>%
      html_nodes(xpath='//*[@class="data lastcolumn"]') %>% .[[8]] %>%
      html_text() %>% 
      enframe(name = NULL) 
    %>% dplyr::select(" " = value)
    )
  output$stat02rsn <- renderTable(
paste("Companies with positive operating cash flow 
      are safer investments than cash burners (negative cash flow).")
, colnames = FALSE
)
  
  output$stat02actn <- renderTable(
    paste('O.K. to buy if Cash Flow is a positive number.')
    , colnames = FALSE
    )
}

# Run the app
shinyApp(ui, server)