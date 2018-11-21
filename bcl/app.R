library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)
library(shinythemes)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(theme = shinytheme("united"),  # shinny theme
  img(src = "bcl_logo.jpg", width = "500", height="180"), # reset the size of picture
  titlePanel("BC Liquor Store Prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      tabsetPanel(    # tab options
        tabPanel("Plot", 
                 colourInput("col","Choose the color of the bars","red"),  # color choice
                 plotOutput("coolplot")),
        tabPanel("Table",
                 checkboxInput("sortInput","Sort by the price",value = FALSE),  # sort by price
                 DT::dataTableOutput("results"))
      )
      
      
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({   # data for plot
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  
  })
  
  selected <- reactive({     # data for table
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    
    if(input$sortInput) {
      bcl %>% 
        arrange(Price)
    }
    else bcl
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    filtered() %>% 
    ggplot(aes(Alcohol_Content)) +
      geom_histogram(fill = input$col)
  })

  output$results <- DT::renderDataTable({
    selected()
  })
  
}

shinyApp(ui = ui, server = server)
