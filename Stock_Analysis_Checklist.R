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

# 1. Gross Margin Trend
# Increasing gross margins signal an improving competitive position, and
# declining margins warn of increasing competition.
# O.K. to buy if the trend is flat or increasing (best).
# Ignore variations of less than 1%, e.g. from 41% to 40.5%.

# Quarterly Income Statement
incstat.qtr <- paste0("https://www.marketwatch.com/investing/Stock/", "FNV", "/financials/income/quarter") %>%
  read_html() %>%
  html_table() %>%
  map_df(bind_cols)
#Subsetting the table into 2 tables
incstat.qtr10 <-   incstat.qtr[is.na(incstat.qtr[,1]),]
incstat.qtr20 <-   incstat.qtr[!is.na(incstat.qtr[,1]),1:6]

#table 1, rearranging columns
incstat.qtr11 <- cbind(incstat.qtr10[8],incstat.qtr10[2:6])

# renaming 1st column
names(incstat.qtr20)[1] <- "account_name"
names(incstat.qtr11)[1] <- "account_name"

# appending the 2 tables from above section
incstat.qtr <- rbind(incstat.qtr20,incstat.qtr11)

# Select account name "Gross Income Growth"
incstat.gig <- incstat.qtr[grep("Gross Income Growth", incstat.qtr[,1]),]

# 2.Revenue (sales) Growth Rate Latest Quarter compared
# to previous quarter & most recent year.
# Compare the most recent quarter's (MRQ) year-overyear sales growth rate to previous
# quarters and to the most recent year.
# Ideally, revenue growth would be accelerating or at least equal to earlier numbers.
# But, it's O.K. to buy if MRQ growth is at least 75% of recent growth numbers

# Yearly income statement
incstat.yr <- paste0("https://www.marketwatch.com/investing/Stock/", "FNV", "/financials") %>%
  read_html() %>%
  html_table() %>%
  map_df(bind_cols)

#Subsetting the table into 2 tables
incstat.yr10 <-   incstat.yr[is.na(incstat.yr[,1]),]
incstat.yr20 <-   incstat.yr[!is.na(incstat.yr[,1]),1:6]

#table 1, rearranging columns
incstat.yr11 <- cbind(incstat.yr10[8],incstat.yr10[2:6])

# renaming 1st column
names(incstat.yr20)[1] <- "account_name"
names(incstat.yr11)[1] <- "account_name"

# appending the 2 tables from above section
incstat.yr <- rbind(incstat.yr20,incstat.yr11)
str(incstat.yr)

# Quarterly sales growth
incstat.revg <- incstat.qtr[grep("Sales Growth", incstat.qtr[,1]),]

# Yearly sales growth
incstat.yr.revg <- incstat.yr[grep("Sales Growth", incstat.yr[,1]),]

#3. Forecast Revenue Growth Rate
# Compare consensus revenue growth forecasts to historical numbers.

key.analysis <- paste0("https://finance.yahoo.com/quote/", stck, "/analysis?p=", stck) %>%
  read_html() %>%
  html_table()


# Revenue Estimate data frame
rev.est <- data.frame(key.analysis[2])

# revenue estimate growth
rev.est.grw <- rev.est[grep("Growth", rev.est[,1]),]

#4. Accounts receivables growth vs sales growth
# Accounts Receivables Ratio (ratio) is the total receivables 
# divided by the revenue for the same quarter
# ratio for the most recent and the year-ago quarters
# Ideally the most recent ratio would be less than yearago, but it's O.K. to
# buy if the ratio is the same or lower than year-ago. 
# Ignore increases that are less than 5%, e.g. from 60% to 64%

# Balance Sheet yearly
bs.yr <- paste0("https://www.marketwatch.com/investing/Stock/", "FNV", "/financials/balance-sheet") %>% 
  read_html() %>% 
  html_table() %>% 
  map_df(bind_cols)

#Subsetting the table into 2 tables
bs.yr10 <-   bs.yr[is.na(bs.yr[,1]),]
bs.yr20 <-   bs.yr[!is.na(bs.yr[,1]),1:6]

#table 1, rearranging columns
bs.yr11 <- cbind(bs.yr10[8],bs.yr10[2:6])

# renaming 1st column on both tables
names(bs.yr20)[1] <- "account_name"
names(bs.yr11)[1] <- "account_name"

## appending the 2 tables from section row 146
bs.yr <- rbind(bs.yr20,bs.yr11)

# Select account name "accounts receivable"
bs.yr.ar <- bs.yr[grep("Total Accounts Receivable", bs.yr[,1]),]

# select revenue from Inc Statement yearly
rev.yr <- incstat.yr[grep("Revenue", incstat.yr[,1]),]

# Balance Sheet quarterly
bs.qtr <- paste0("https://www.marketwatch.com/investing/Stock/", "FNV", "/financials/balance-sheet") %>% 
  read_html() %>% 
  html_table() %>% 
  map_df(bind_cols)

#Subsetting the table into 2 tables
bs.qtr10 <-   bs.qtr[is.na(bs.qtr[,1]),]
bs.qtr20 <-   bs.qtr[!is.na(bs.qtr[,1]),1:6]

#table 1, rearranging columns
bs.qtr11 <- cbind(bs.qtr10[8],bs.qtr10[2:6])

# renaming 1st column on both tables
names(bs.qtr20)[1] <- "account_name"
names(bs.qtr11)[1] <- "account_name"

## appending the 2 tables from section row 146
bs.qtr <- rbind(bs.qtr20,bs.qtr11)

# Select account name "accounts receivable"
bs.qtr.ar <- bs.qtr[grep("Total Accounts Receivable", bs.qtr[,1]),]

# select revenue from Inc Statement yearly
rev.qtr <- incstat.qtr[grep("Revenue", incstat.qtr[,1]),]
