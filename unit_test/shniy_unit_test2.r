setwd("~/R/stockVis")
# Load packages ----
library(shiny)
library(tidyverse)
library(quantmod)
library(xml2)
library(rvest)
library(tibble)
library(DT)

# Source helpers ----
# source("helpers.R")
#source("Stock_Analysis_Checklist_shiny.R", local = TRUE)
# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.
               
               Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "FNV"),
      
      # table output
    ),
    mainPanel(
      tableOutput("tableDT")
      )
  )
)
  # Server logic
  server <- function(input, output) {

    output$tableDT <- renderTable(
      paste0("https://www.marketwatch.com/investing/Stock/", stck, "/financials/balance-sheet/quarter") %>% 
        read_html() %>% 
        html_table() %>% 
        map_df(bind_cols) 
      %>%
        select(1:5) %>%
        slice(11)
    )

    }
  
shinyApp(ui = ui, server = server)
  