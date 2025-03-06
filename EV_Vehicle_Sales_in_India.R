library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ev_data <- read.csv("C:/Users/monis/OneDrive/Desktop/Ev_Sales_In_India.csv")

# UI
display_ui <- fluidPage(
  titlePanel("EV Vehicle Sales in India (2017-2022)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graph_type", "Select Graph Type:",
                  choices = c("Line Chart", "Stacked Bar Chart"))
    ),
    mainPanel(
      plotOutput("ev_plot")
    )
  )
)

# Server
display_server <- function(input, output) {
  output$ev_plot <- renderPlot({
    
    ev_long <- ev_data %>% pivot_longer(cols = -Year, names_to = "Category", values_to = "Sales")
    
    if (input$graph_type == "Line Chart") {
      ggplot(ev_long, aes(x = Year, y = Sales, color = Category)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "EV Sales Trend (2017-2022)", x = "Year", y = "Sales") +
        theme_minimal()
      
    } else {
      ggplot(ev_long, aes(x = as.factor(Year), y = Sales, fill = Category)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = "EV Sales Distribution by Category", x = "Year", y = "Proportion") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
    }
  })
}

shinyApp(ui = display_ui, server = display_server)
