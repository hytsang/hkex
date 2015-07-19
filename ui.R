library(shiny)
library(data.table)

shinyUI(
    fluidPage(
        titlePanel("Insider Trading"),
        sidebarLayout(
            sidebarPanel(
                selectInput("whichtable",
                            "Who?",
                            choices = list(Directors = 1, Shareholders = 2, Everyone = 3),
                            selected = 3),
                sliderInput("changethreshold",
                            "Threshold for change in shareholding (%)",
                            min = 0,
                            max = 5,
                            step = 0.01,
                            value = 1),
                sliderInput("amountthreshold",
                            "Threshold for change in amount (HK$)",
                            min = 0,
                            max = 10^7,
                            step = 5*10^5,
                            value = 10^6),
                dateRangeInput("daterange",
                               label = 'Date range:',
                               start = Sys.Date() - 30, end = Sys.Date(), max = Sys.Date(), min = Sys.Date() - 365
                               )
            ),
            
            mainPanel(
                dataTableOutput("tablenet")
            )
        )
    )
)
        
