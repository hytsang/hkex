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

hkexsmall <- read.csv("hkexsmall.csv", na.strings = c("", "NA"))

# Define UI
ui <- fluidPage(
  titlePanel("Insider Trading"),
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
                  "Minimum shareholding threshold change (%)",
                  min = -100,
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
      noticestablenet <- hkexsmall %>% filter(position == input$position) %>% filter(formtype == input$formtype) %>% filter(dmy(date) >= ymd(as.character(input$daterange[1])) & dmy(date) <= ymd(as.character(input$daterange[2]))) %>% arrange(corporation, stock_code, canonicalname, desc(when), date) %>% group_by(corporation, stock_code, canonicalname) %>% summarise(sharesdiff = sum(diff(value))) %>% filter(abs(sharesdiff) >= input$changethreshold & abs(sharesdiff - input$changethreshold) <= abs(sharesdiff)) %>% ungroup
      noticestablenet <- noticestablenet %>% mutate(stock_code = paste0('<a href="http://www.aastocks.com/en/ltp/rtquote.aspx?symbol=', stock_code, '" target="_blank">', stock_code, '</a>'))
    # setnames(noticestablenet, c("Stock code", "Name", "Company", "Change in long position", "Change in short position", "Change in lending pool", "Total amount of money (HK$)"))
      noticestablenet
  }, escape = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

