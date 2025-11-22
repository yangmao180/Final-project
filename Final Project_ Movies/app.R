
# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(viridis)
library(treemapify)
library(networkD3)
library(scales)
library(lubridate)
library(ggridges)
library(rsconnect)
# Load data and functions
source("data_explorer.R")
movies_clean <- readRDS("movies_clean.rds")

# Generate all components
components <- run_data_explorer("movies_clean.rds")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "MovieBox Prophet - Data Analysis Dashboard",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database")),
      menuItem("Genre Analysis", tabName = "genre", icon = icon("tags")),
      menuItem("Time Trends", tabName = "time", icon = icon("chart-line")),
      menuItem("Top Performers", tabName = "top", icon = icon("trophy")),
      menuItem("Network Analysis", tabName = "network", icon = icon("project-diagram")),
      menuItem("Interactive Charts", tabName = "interactive", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    # Add custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .info-box {
          border-radius: 10px;
        }
        .small-box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          infoBox(
            "Total Movies", 
            components$overview$total_movies, 
            icon = icon("film"),
            color = "blue",
            width = 3
          ),
          infoBox(
            "Total Revenue", 
            components$overview$total_revenue, 
            icon = icon("dollar-sign"),
            color = "green",
            width = 3
          ),
          infoBox(
            "Average Rating", 
            paste0(components$overview$avg_rating, " ★"), 
            icon = icon("star"),
            color = "yellow",
            width = 3
          ),
          infoBox(
            "Date Range", 
            components$overview$date_range, 
            icon = icon("calendar"),
            color = "purple",
            width = 3
          )
        ),
        fluidRow(
          box(
            title = "Genre Universe: Box Office Dominance",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("genre_treemap", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "Box Office Distribution by Era",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("box_office_ridges", height = "400px")
          ),
          box(
            title = "Rating Evolution by Decade",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("rating_evolution", height = "400px")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Interactive Movie Database",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        )
      ),
      
      # Genre Analysis Tab
      tabItem(
        tabName = "genre",
        fluidRow(
          box(
            title = "Genre Combination Success Matrix",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("genre_combos", height = "500px")
          ),
          box(
            title = "Seasonal Movie Release Patterns",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("seasonal", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Box Office Distribution (Log Scale)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("box_office_histogram", height = "400px")
          )
        )
      ),
      
      # Time Trends Tab
      tabItem(
        tabName = "time",
        fluidRow(
          box(
            title = "Movie Industry Trends Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("time_series", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Switchable Timeline Metrics",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("switchable_timeline", height = "400px")
          )
        )
      ),
      
      # Top Performers Tab
      tabItem(
        tabName = "top",
        fluidRow(
          box(
            title = "Top 20 Movies by Box Office Revenue (Inflation-Adjusted)",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("top_revenue", height = "600px")
          ),
          box(
            title = "Top 20 Highest Rated Movies",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_rating", height = "600px")
          )
        )
      ),
      
      # Network Analysis Tab
      tabItem(
        tabName = "network",
        fluidRow(
          box(
            title = "Director-Actor Collaboration Network",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            forceNetworkOutput("network", height = "700px")
          )
        )
      ),
      
      # Interactive Charts Tab
      tabItem(
        tabName = "interactive",
        fluidRow(
          box(
            title = "Genre Revenue Flow by Decade",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("genre_flow", height = "500px")
          ),
          box(
            title = "Genre Performance by Release Month",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("interactive_heatmap", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "3D Movie Space Explorer",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("movie_3d", height = "600px")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Overview Tab Outputs
  output$genre_treemap <- renderPlot({
    components$genre_treemap
  })
  
  output$box_office_ridges <- renderPlot({
    components$box_office_dist$ridges
  })
  
  output$rating_evolution <- renderPlot({
    components$rating_evolution
  })
  
  # Data Explorer Tab
  output$data_table <- renderDT({
    components$data_table
  })
  
  # Genre Analysis Tab
  output$genre_combos <- renderPlot({
    components$genre_combos
  })
  
  output$seasonal <- renderPlot({
    components$seasonal
  })
  
  output$box_office_histogram <- renderPlot({
    components$box_office_dist$histogram
  })
  
  # Time Trends Tab
  output$time_series <- renderPlotly({
    components$time_series
  })
  
  output$switchable_timeline <- renderPlotly({
    create_switchable_timeline(movies_clean)
  })
  
  # Top Performers Tab
  output$top_revenue <- renderPlot({
    components$top_performers$revenue
  })
  
  output$top_rating <- renderPlotly({
    components$top_performers$rating
  })
  
  # Network Analysis Tab
  output$network <- renderForceNetwork({
    components$network
  })
  
  # Interactive Charts Tab
  output$genre_flow <- renderPlotly({
    create_genre_flow_diagram(movies_clean)
  })
  
  output$interactive_heatmap <- renderPlotly({
    create_interactive_heatmap(movies_clean)
  })
  
  output$movie_3d <- renderPlotly({
    create_3d_movie_space(movies_clean)
  })
}

# Run the application
shinyApp(ui = ui, server = server)