# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)



ui <- fluidPage(
  
  # title
  titlePanel("Cricket Team Performance Comparison"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      #  CSV file path
      textInput("file_path", "Enter CSV File Path", 
                value = "C:/Users/monis/OneDrive/Desktop/IPL MATCHES  DATA.csv"),
      
      # Dropdown to select the type of graph
      selectInput("graph_type", "Choose Graph Type", 
                  choices = c("Stacked Bar Plot", "Dumbbell Chart"))
    ),
    
    # Main panel 
    mainPanel(
      plotOutput("performance_plot")  # Output for selected plot type
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to load data when the file path is provided
  data <- reactive({
    req(input$file_path)  # Ensure file path is provided
    
    # Try to read the CSV file and handle errors if the path is incorrect
    tryCatch({
      df <- read.csv(input$file_path)
      
    
      if(!all(c("Teams", "home_wins", "away_wins", "home_matches", "away_matches") %in% colnames(df))) {
        showNotification("CSV file must contain the following columns: Teams, home_wins, away_wins, home_matches, away_matches.", type = "error")
        return(NULL)
      }
      
      return(df)
    }, error = function(e) {
      # If an error occurs (e.g., incorrect file path), show an error message
        showNotification("Error reading CSV file. Please check the file path.", type = "error")
      return(NULL)
    })
  })
  
  # Render Stacked Bar Plot or Dumbbell Chart
  output$performance_plot <- renderPlot({
    
    df <- data()  # Load the data
    
   
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Stacked Bar Plot for Home and Away Wins
    if (input$graph_type == "Stacked Bar Plot") {
      df_long <- df %>%
        pivot_longer(cols = c(home_wins, away_wins), 
                     names_to = "Match_Type", 
                     values_to = "Wins") %>%
        mutate(Match_Type = factor(Match_Type, levels = c("home_wins", "away_wins")))
      
      ggplot(df_long, aes(x = Teams, y = Wins, fill = Match_Type)) +
        geom_bar(stat = "identity") +
        labs(title = "Home vs Away Wins by Team", x = "Teams", y = "Wins") +
        scale_fill_manual(values = c("blue", "red"), labels = c("Home Wins", "Away Wins")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # Dumbbell Chart for comparing Home Wins and Away Wins
    
    else if (input$graph_type == "Dumbbell Chart") {
      # Create a Dumbbell Chart comparing home_wins and away_wins for each team using ggplot2
      ggplot(df, aes(x = home_wins, xend = away_wins, y = Teams)) +
        geom_segment(aes(x = home_wins, xend = away_wins, y = Teams, yend = Teams), color = "gray") +
        geom_point(aes(x = home_wins), color = "green", size = 7) +
        geom_point(aes(x = away_wins), color = "red", size = 4) +
        labs(title = "Home vs Away Wins by Team", x = "Number of Wins", y = "Teams") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
