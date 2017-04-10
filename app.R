#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(shiny)
library(DT)

options("scipen"=100, "digits"=3)

hkexsmall <- tbl(src_postgres("hkdata"), "hkex")

# Define UI
ui <- fluidPage(
  titlePanel("HKEX Insider Trading"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("position",
                   "Position",
                   choices = c("Long" = "Long Position",
                               "Short" = "Short Position",
                               "Lending pool" = "Lending Pool")
      ),
      radioButtons("formtype",
                   "Shareholder or director?",
                   choices = c("Individual" = "1",
                               "Corporate shareholder" = "2",
                               "Director" = "3A")
      ),
      sliderInput("changethreshold",
                  "Minimum change in position (%)",
                  min = 0,
                  max = 100,
                  step = 1,
                  value = 0
      ),
      dateRangeInput("daterange",
                     label = 'Date range:',
                     start = Sys.Date() - 365, 
                     end = Sys.Date(), 
                     min = "2003-04-01",
                     max = Sys.Date()
      )
    ),
    mainPanel(
      dataTableOutput("tablenet")
    )
  )
)

# Define server logic
server <- function(input, output) {
   
  output$tablenet <- renderDataTable({
      noticestablenet <- hkexsmall %>% filter(position == input$position) %>% filter(formtype == input$formtype) %>% filter(date >= input$daterange[1] & date <= input$daterange[2]) %>% arrange(corporation, stock_code, canonicalname, desc(beforeafter), date) %>% group_by(corporation, stock_code, canonicalname) %>% filter(row_number() == 1 | row_number() == n()) %>% mutate(sharesdiff = value - first(value)) %>% filter(beforeafter == "sharesafter") %>% filter(abs(sharesdiff) >= input$changethreshold) %>% ungroup %>% select(corporation, stock_code, canonicalname, sharesdiff) %>% collect
      noticestablenet <- noticestablenet %>% mutate(stock_code = paste0('<a href="http://www.aastocks.com/en/ltp/rtquote.aspx?symbol=', stock_code, '" target="_blank">', stock_code, '</a>'))
      noticestablenet %>% datatable(escape = FALSE, rownames = FALSE, colnames = c("Listed company", "Stock code", "Shareholder/director name", "Change in position (%)")) %>% formatRound(columns = "sharesdiff", digits = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

