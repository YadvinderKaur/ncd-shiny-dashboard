library(shiny)
library(ggplot2)
library(dplyr)

health_data <- read.csv("health_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("NCD Risk Factors Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "region",
        "Choose a region",
        choices = c("All", sort(unique(health_data$region))),
        selected = "All"
      )
    ),
    
    mainPanel(
      plotOutput("plot1"),
      tableOutput("table1")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$region == "All") {
      health_data
    } else {
      filter(health_data, region == input$region)
    }
  })
  
  output$plot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = obesity_rate, y = diabetes_rate)) +
      geom_point() +
      geom_text(aes(label = country), vjust = -0.7, size = 3) +
      labs(x = "Obesity rate (%)", y = "Diabetes rate (%)") +
      theme_minimal()
  })
  
  output$table1 <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
