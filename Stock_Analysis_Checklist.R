# From this source
# http://www.winninginvesting.com/stock_analysis_checklist.htm

library(quantmod)
library(tidyquant)
library(rvest)
library(xml2)
library(glue)

# tidyquant: https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ01-core-functions-in-tidyquant.html

## Part 1: Basic Research ##
# 1. One-year Price chart with 50-day moving average
# List all available metrics from Yahoo finance
# O.K. to buy if stock price is above its 50-day moving average.

company <- c("FNV")

# use getsymbols() to convert a char
stck <- getSymbols(company)

# charting
stockEnv <- new.env()
symbols <- getSymbols(stck, src='yahoo', env=stockEnv)

# O.K. to buy if stock price is above its 50-day moving average.

for (stock in ls(stockEnv)){
chartSeries(stockEnv[[stock]], theme="white", name=stock,
TA="addVo(); addSMA(50, col='black')", subset='last 365 days')
}

# 2. Price/Sales ratio (P/S)
# O.K. to buy if P/S is less than 10. 
# P/S ratios between 3 and 5 are best for growth stocks.
# Ratios below 2 reflect value priced stocks.  

key.statistics <- paste0("https://finance.yahoo.com/quote/", stck, "/key-statistics?p=", stck) %>%
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
# Add stock name column
key.statistics$Stock_Name <- stck
key.statistics[6]

# 3. Cash Flow
# O.K. to buy if Cash Flow is a positive number.

cash.flow <- glue("https://www.marketwatch.com/investing/Stock/",{company},"/profile/") %>%
  read_html() %>%
  html_nodes(xpath='//*[@class="sixwide addgutter"]') %>%
  html_nodes(xpath='//*[@class="section"]') %>%
#   html_nodes(xpath='//*[@class="column"]') %>% .[[10]] %>%
#  html_text() %>%
html_nodes(xpath='//*[@class="data lastcolumn"]') %>% .[[8]]%>%
html_text()

cash.flow

# 4. Average Daily Volume (shares)
# O.K. to buy if Average Daily Volume is 150,000 shares or higher, and above one million shares is best.
key.statistics[17]


# 5. Fundamental Health Grade
# www.Investorplace.com

stock_health  <- paste("https://investorplace.com/stock-quotes/", stck, "-stock-quote/") %>%
read_html() %>%
html_nodes(xpath='//*[@class="stock-aside"]') %>%
html_nodes(xpath='//*[@class="grade grade-color"]') %>%
html_nodes(xpath='//*[@class="grade-color stock-grade-rating"]') %>%
html_text()

stock_health

# 6. Got Growth?
# Best case is when year-over-year (YoY) growth is accelerating.
key.statistics[46]

# 7.Institutional Ownership
# O.K. to buy if percent held by institutions is at least 30% of shares outstanding.
key.statistics[22]

# 8. Number of Analysts Making Buy/Hold/Sell Recs.
# O.K. to buy if a total of at least 4 analysts are listed 
# as currently making strong buy, buy, hold, underperform, or sell recommendations. 
# Look only at the total number of analysts making recommendations, 
# not whether there are more buys than holds, etc. 

# Extract and transform data
df.mw <- paste0("https://www.marketwatch.com/investing/Stock/", stck, "/analystestimates") %>%
  read_html() %>%
  html_table() %>%
  map_df(bind_cols) %>%
  # Transpose
  t() %>%
  as_tibble()

df.mw <- df.mw[1:2,1:5]
# Set first row as column names
colnames(df.mw) <- df.mw[1,]
# Remove first row
df.mw <- df.mw[-1,]
# Add stock name column
df.mw$Stock_Name <- stock
df.mw[2]
df.mw[1]

## Part 2: Advanced Research & Analysis ##
