setwd("~/R/stockVis")
# Load packages ----
library(shiny)
library(shinythemes)
library(quantmod)
library(tidyverse)
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
  tabPanel("Research",
           sidebarLayout(
             sidebarPanel(
               helpText("Select a stock to examine. 
                        Information will be collected from Yahoo Finance, Market Watch and InvestorPlace."),
               textInput("symb", "Symbol", "FNV"),
               tableOutput("cmpname"),
               tableOutput("cmpdesc")
               ),
             mainPanel(
               tabsetPanel(
                 # tab panel 1
                 tabPanel("Basic Research",
               # Basic Analysis 
               # 1
               tags$p()
               , tags$strong("1. One-year Price chart with 50-day moving average")
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
               , tags$p()
               # 4
               , tags$strong("4. Average Daily Volume (shares)")
               , tags$p()
               , tableOutput("stat03")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat03rsn")
               , tags$strong("Action")
               , tableOutput("stat03actn")
               , tags$p()
               # 5
               , tags$strong("5. Fundamental Health Grade")
               , tags$p()
               , tableOutput("stat04")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat04rsn")
               , tags$strong("Action")
               , tableOutput("stat04actn")
               , tags$p()
               # 6
               , tags$strong("6. Got Growth?")
               , tags$p()
               , tableOutput("stat05")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat05rsn")
               , tags$strong("Action")
               , tableOutput("stat05actn")
               , tags$p()
               # 7
               , tags$strong("7. Institutional Ownership")
               , tags$p()
               , tableOutput("stat06")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat06rsn")
               , tags$strong("Action")
               , tableOutput("stat06actn")
               , tags$p()
               # 8
               , tags$strong("8. Number of Analysts Making Buy/Hold/Sell Recs")
               , tags$p()
               , tableOutput("stat07")
               , tags$p()
               , tags$strong("Reason")
               , tableOutput("stat07rsn")
               , tags$strong("Action")
               , tableOutput("stat07actn")
               , tags$p()
               
               ),
               
               # tab panel 2
               tabPanel("Advanced Research",
                        tags$p()
                        ,tags$strong("1. Gross Margin Trend")
                 , tags$p()
                 , tableOutput("advstat01")
                 , tags$p()
                 , tags$strong("Reason")
                 , tableOutput("advstat01rsn")
                 , tags$strong("Action")
                 , tableOutput("advstat01actn")
                 , tags$p()
                 # 2
                 , tags$strong("Revenue (sales) Growth Rate Latest Quarter compared
                               to previous quarter & most recent year.")
                 , tags$p()
                 , tableOutput("advstat02")
                 , tags$p()
                 , tags$strong("Reason")
                 , tableOutput("advstat02rsn")
                 , tags$strong("Action")
                 , tableOutput("advstat02actn")
                 , tags$p()
                 # 3
                 , tags$strong("3. Forecast Revenue Growth Rate")
                 , tags$p()
                 , tableOutput("advstat03")
                 , tags$p()
                 , tags$strong("Reason")
                 , tableOutput("advstat03rsn")
                 , tags$strong("Action")
                 , tableOutput("advstat03actn")
                 , tags$p()
                # 4
                 , tags$strong("4. Accounts receivables growth vs sales growth")
                 , tags$p()
                 , tableOutput("advstat04")
                 , tags$p()
                 , tags$strong("Reason")
                 , tableOutput("advstat04rsn")
                 , tags$strong("Action")
                 , tableOutput("advstat04actn")
                 , tags$p()
                 
                 )
               )
             )
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
  
  ## Basic Analysis - key statistics
  # Price to Sales
  output$stat01 <- renderTable(
    paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
      read_html() %>%
      html_table() %>%
      map_df(bind_cols) %>%
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
  # Avergae daily shares
  output$stat03 <- renderTable(
    paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
      read_html() %>%
      html_table() %>%
      map_df(bind_cols) %>%
      # Transpose
      t() %>%
      as_tibble()
    %>% dplyr::select(" " = V17)

  )
  output$stat03rsn <- renderTable(
    paste("Institutional buying is an important catalyst for share price growth.
          Institutions buy hundreds of thousands of shares and prefer stocks with
          large daily trading volumes so they can easily move in and out of positions.")
    , colnames = FALSE
    )

  output$stat03actn <- renderTable(
    paste('O.K. to buy if Average Daily Volume is 150,000 shares or higher, and above one million shares is best.')
    , colnames = FALSE
  )
  # Health grade
  output$stat04 <- renderTable(
    paste("https://investorplace.com/stock-quotes/", input$symb, "-stock-quote/") %>%
      read_html() %>%
      html_nodes(xpath='//*[@class="stock-aside"]') %>%
      html_nodes(xpath='//*[@class="grade grade-color"]') %>%
      html_nodes(xpath='//*[@class="grade-color stock-grade-rating"]') %>%
      html_text() %>%
      enframe(name = NULL)

  )
  output$stat04rsn <- renderTable(
    paste("Invest, don’t gamble! Stick with companies with solid fundamentals.")
    , colnames = FALSE
  )

  output$stat04actn <- renderTable(
    paste('O.K. to buy if Fundamental Grade = A, B or C.')
    , colnames = FALSE
  )
  # # Got growth?
  output$stat05 <- renderTable(
    paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
      read_html() %>%
      html_table() %>%
      map_df(bind_cols) %>%
      # Transpose
      t() %>%
      as_tibble()
    %>% dplyr::select(" " = V46)

  )
  output$stat05rsn <- renderTable(
    paste("Consistent strong sales growth over extended periods translates
          to long term stock price appreciation.")
    , colnames = FALSE
    )

  output$stat05actn <- renderTable(
    paste("O.K. to buy if recent quarterly growth numbers are 8% min. (higher is better).
          Best case is when year-over-year (YoY) growth is accelerating.")
    , colnames = FALSE
    )
  # # Institutional ownership
  output$stat06 <- renderTable(
    paste0("https://finance.yahoo.com/quote/", input$symb, "/key-statistics?p=", input$symb) %>%
      read_html() %>%
      html_table() %>%
      map_df(bind_cols) %>%
      # Transpose
      t() %>%
      as_tibble()
    %>% dplyr::select(" " = V22)

  )
  output$stat06rsn <- renderTable(
    paste("Lack of institutional ownership means mutual funds, pension plans and other institutional
          buyers don’t think they will make money owning the stock.
          Why would you want to own it?")
    , colnames = FALSE
    )

  output$stat06actn <- renderTable(
    paste("O.K. to buy if percent held by institutions is at
          least 30% of shares outstanding.")
    , colnames = FALSE
    )
  # # number of analyst making recommendations
  output$stat07 <- renderTable(
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

  output$stat07rsn <- renderTable(
    paste("A company’s performance will go unrewarded if nobody knows about it.
          Sufficient analyst coverage is essential to create investor interest,
          especially from institutions.")
    , colnames = FALSE
    )

  output$stat07actn <- renderTable(
    paste("O.K. to buy if a total of at least 4 analysts are listed as currently making strong buy, buy, hold,
          underperform, or sell recommendations. Look only at the total number of analysts making recommendations,
          not whether there are more buys than holds, etc.")
    , colnames = FALSE
    )

  
    
  ## Advanced Analysis - key statistics
  # Gross Margin Trend
  output$advstat01 <- renderTable(
    paste0("https://www.marketwatch.com/investing/Stock/", input$symb, "/financials/income/quarter") %>%
      read_html() %>%
      html_table() %>%
      map_df(dplyr::bind_cols)%>%
      select(1:5) %>%
      slice(10)
    
  )
  output$advstat01rsn <- renderTable(
    paste("Increasing gross margins signal an improving competitive position,
          and declining margins warn of increasing competition.")
    , colnames = FALSE
  )
  
  output$advstat01actn <- renderTable(
    paste("O.K. to buy if the trend is flat or increasing (best).
          Ignore variations of less than 1%, e.g. from 41% to 40.5%.")
    , colnames = FALSE
  )
  
    # Revenue growth rate latest quarter compared
    output$advstat02 <- renderTable(
      paste0("https://www.marketwatch.com/investing/Stock/", input$symb, "/financials") %>%
        read_html() %>%
        html_table() %>%
        map_df(bind_cols) %>%
        select(1:5) %>%
        slice(2)

        )

    output$advstat02rsn <- renderTable(
      paste("Compare the most recent quarter's (MRQ) year-overyear sales growth
  rate to previous quarters and to the most recent year.")
      , colnames = FALSE
      )

    output$advstat02actn <- renderTable(
      paste("Ideally, revenue growth would be accelerating or at least equal to earlier numbers.
             But, it's O.K. to buy if MRQ growth is at least 75% of recent growth numbers.")
      , colnames = FALSE
    )
    # Forecast revenue growth rate
    output$advstat03 <- renderTable(
      paste0("https://finance.yahoo.com/quote/", input$symb, "/analysis?p=", input$symb) %>%
        read_html() %>%
        html_table() %>%
        .[[2]] %>%
        slice(6)

    )

    output$advstat03rsn <- renderTable(
      paste("Compare consensus revenue growth forecasts to historical numbers.")
      , colnames = FALSE
      )

    output$advstat03actn <- renderTable(
      paste("Ideally, the growth rate should be accelerating but it's O.K. to buy
            if the forecast year-overyear revenue growth is at least
            75% of historical year-over-year growth.")
      , colnames = FALSE
    )
    # Accounts receivables growth vs sales growth
   #  output$advstat04 <- renderTable(
   # 
   #  )
   # 
   #  output$advstat04rsn <- renderTable(
   #    paste("Accounts Receivables Ratio (ratio) is the total receivables
   # divided by the revenue for the same quarter
   # ratio for the most recent and the year-ago quarters.")
   #    , colnames = FALSE
   #  )
   # 
   #  output$advstat04actn <- renderTable(
   #    paste("Ideally the most recent ratio would be less than year ago, but it's O.K. to
   #          buy if the ratio is the same or lower than year-ago.
   #          Ignore increases that are less than 5%, e.g. from 60% to 64%.")
   #    , colnames = FALSE
   #  )
  
  }

# Run the app
shinyApp(ui, server)