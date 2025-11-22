# Load required libraries
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(treemapify)
library(ggwordcloud)
library(networkD3)
library(scales)
library(lubridate)
library(ggridges)

# 1. DATASET OVERVIEW CARDS
create_overview_cards <- function(data) {
  # Calculate key statistics using adjusted values
  total_movies <- nrow(data)
  total_revenue <- sum(data$BoxOffice_adjusted, na.rm = TRUE)  # Using adjusted
  avg_rating <- mean(data$imdbRating, na.rm = TRUE)
  date_range <- paste(min(data$Year, na.rm = TRUE), "-", max(data$Year, na.rm = TRUE))
  
  # Extract unique counts
  all_directors <- unlist(str_split(data$Director, ", "))
  unique_directors <- n_distinct(all_directors[!is.na(all_directors)])
  
  all_genres <- unlist(str_split(data$Genre, ", "))
  unique_genres <- n_distinct(all_genres[!is.na(all_genres)])
  
  # Return as list for display
  list(
    total_movies = comma(total_movies),
    total_revenue = paste0(dollar(total_revenue), " (2025 adj.)"),  # Indicate adjusted
    avg_rating = round(avg_rating, 2),
    date_range = date_range,
    unique_directors = comma(unique_directors),
    unique_genres = unique_genres
  )
}

# 2. INTERACTIVE DATA TABLE
create_data_table <- function(data) {
  # Prepare table data with both nominal and adjusted values
  table_data <- data %>%
    select(Title, Year, Genre, Director, BoxOffice_num, BoxOffice_adjusted, 
           imdbRating, imdbVotes, Runtime_num, inflation_multiplier) %>%
    mutate(
      # Format nominal box office
      BoxOffice_Nominal = case_when(
        is.na(BoxOffice_num) ~ "N/A",
        BoxOffice_num == 0 ~ "N/A",
        BoxOffice_num < 1000000 ~ paste0("$", format(round(BoxOffice_num/1000), big.mark = ","), "K"),
        BoxOffice_num < 1000000000 ~ paste0("$", format(round(BoxOffice_num/1000000, 1), big.mark = ","), "M"),
        TRUE ~ paste0("$", format(round(BoxOffice_num/1000000000, 2), big.mark = ","), "B")
      ),
      # Format adjusted box office
      BoxOffice_Adj = case_when(
        is.na(BoxOffice_adjusted) ~ "N/A",
        BoxOffice_adjusted == 0 ~ "N/A",
        BoxOffice_adjusted < 1000000 ~ paste0("$", format(round(BoxOffice_adjusted/1000), big.mark = ","), "K"),
        BoxOffice_adjusted < 1000000000 ~ paste0("$", format(round(BoxOffice_adjusted/1000000, 1), big.mark = ","), "M"),
        TRUE ~ paste0("$", format(round(BoxOffice_adjusted/1000000000, 2), big.mark = ","), "B")
      ),
      # Format rating
      IMDb_Rating = round(imdbRating, 1),
      # Format other columns
      Votes = format(imdbVotes, big.mark = ","),
      Runtime = paste(Runtime_num, "min"),
      # Inflation factor
      `Inflation Factor` = paste0("×", inflation_multiplier)
    ) %>%
    select(Title, Year, Genre, Director, BoxOffice_Nominal, BoxOffice_Adj, 
           `Inflation Factor`, IMDb_Rating, Votes, Runtime)
  
  # Create data table
  datatable(
    table_data,
    filter = 'top',
    rownames = FALSE,
    options = list(
      pageLength = 25,
      searchHighlight = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(className = 'dt-center', targets = c(1, 6, 7, 8, 9)),
        list(className = 'dt-right', targets = c(4, 5))
      )
    ),
    colnames = c('Title', 'Year', 'Genres', 'Director(s)', 
                 'Box Office (Nominal)', 'Box Office (2024 adj.)', 
                 'Inflation', 'IMDb Rating', 'Votes', 'Runtime')
  ) %>%
    formatStyle(
      'IMDb_Rating',
      background = styleColorBar(table_data$IMDb_Rating, 'lightblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
}

# 3. GENRE UNIVERSE TREEMAP - Using adjusted values
create_genre_treemap <- function(data) {
  genre_stats <- data %>%
    separate_rows(Genre, sep = ", ") %>%
    group_by(Genre) %>%
    summarise(
      total_revenue = sum(BoxOffice_adjusted, na.rm = TRUE),  # Using adjusted
      movie_count = n(),
      avg_rating = mean(imdbRating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(movie_count >= 10) %>%
    mutate(
      label = paste(Genre, "\n",
                    "$", round(total_revenue/1e9, 1), "B\n",
                    movie_count, " movies\n",
                    "★", round(avg_rating, 1))
    )
  
  ggplot(genre_stats, aes(area = total_revenue, fill = avg_rating, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white", place = "centre", size = 12) +
    scale_fill_viridis(name = "Avg Rating", option = "plasma") +
    labs(title = "Genre Universe: Box Office Dominance (Inflation-Adjusted)",
         subtitle = "Size = Total Revenue (2024 dollars) | Color = Average Rating") +
    theme_minimal()
}

# 4. BOX OFFICE DISTRIBUTION - Show both nominal and adjusted
create_box_office_distribution <- function(data) {
  # Adjusted distribution
  p1 <- data %>%
    filter(BoxOffice_adjusted > 0) %>%
    ggplot(aes(x = BoxOffice_adjusted/1e6)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
    scale_x_log10(
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.1", "1", "10", "100", "1K", "10K")
    ) +
    labs(title = "Box Office Distribution - Inflation Adjusted (2024 dollars)",
         x = "Box Office (Million USD)",
         y = "Number of Movies") +
    theme_minimal()
  
  # Ridge plot by era - using adjusted values
  p2 <- data %>%
    filter(BoxOffice_adjusted > 0) %>%
    mutate(era = factor(era, levels = c("Classic Era", "Blockbuster Era", 
                                        "Digital Era", "Streaming Era"))) %>%
    ggplot(aes(x = BoxOffice_adjusted/1e6, y = era, fill = era)) +
    geom_density_ridges(alpha = 0.8, scale = 2) +
    scale_x_log10(
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = c("$0.1M", "$1M", "$10M", "$100M", "$1B", "$10B")
    ) +
    scale_fill_viridis_d() +
    labs(title = "Box Office Distribution by Era (Inflation-Adjusted)",
         x = "Box Office Revenue (2024 dollars, log scale)",
         y = "") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(list(histogram = p1, ridges = p2))
}

# 5. RATING EVOLUTION - No change needed
create_rating_evolution <- function(data) {
  rating_plot <- data %>%
    filter(!is.na(decade), decade >= 1970) %>%
    mutate(decade = factor(decade)) %>%
    ggplot(aes(x = imdbRating, y = decade, fill = decade)) +
    geom_density_ridges(alpha = 0.8, scale = 2.5) +
    scale_fill_viridis_d() +
    labs(title = "Evolution of Movie Ratings by Decade",
         subtitle = "How audience standards have changed over time",
         x = "IMDb Rating",
         y = "Decade") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(rating_plot)
}

# 6. SEASONAL PATTERNS - Using adjusted values
create_seasonal_analysis <- function(data) {
  seasonal_data <- data %>%
    filter(!is.na(Released)) %>%
    mutate(
      month = month(Released, label = TRUE),
      week = week(Released),
      quarter = quarter(Released)
    ) %>%
    group_by(month) %>%
    summarise(
      avg_revenue = mean(BoxOffice_adjusted, na.rm = TRUE)/1e6,  # Using adjusted
      movie_count = n(),
      avg_rating = mean(imdbRating, na.rm = TRUE),
      blockbuster_pct = sum(BoxOffice_adjusted > 500000000, na.rm = TRUE) / n() * 100,  # Adjusted threshold
      .groups = "drop"
    )
  
  polar_plot <- seasonal_data %>%
    ggplot(aes(x = month)) +
    geom_col(aes(y = avg_revenue, fill = avg_rating), width = 0.9) +
    geom_text(aes(y = avg_revenue + 5, label = movie_count), size = 3) +
    coord_polar() +
    scale_fill_viridis(name = "Avg Rating") +
    labs(title = "Seasonal Movie Release Patterns (Inflation-Adjusted)",
         subtitle = "Bar height = Average Revenue (2024 dollars) | Numbers = Movie Count",
         x = "", y = "Average Revenue (Million USD)") +
    theme_minimal() +
    theme(axis.text.y = element_blank())
  
  return(polar_plot)
}

# 7. DIRECTOR-ACTOR NETWORK - Using adjusted values for collaboration weight
create_collaboration_network <- function(data, min_collaborations = 3) {
  collaborations <- data %>%
    mutate(
      primary_director = str_split(Director, ", ", simplify = TRUE)[,1],
      primary_actor = str_split(Actors, ", ", simplify = TRUE)[,1]
    ) %>%
    filter(!is.na(primary_director), !is.na(primary_actor),
           primary_director != "", primary_actor != "") %>%
    group_by(primary_director, primary_actor) %>%
    summarise(
      collaborations = n(),
      total_revenue = sum(BoxOffice_adjusted, na.rm = TRUE),  # Using adjusted
      avg_rating = mean(imdbRating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(collaborations >= min_collaborations) %>%
    arrange(desc(total_revenue)) %>%  # Sort by adjusted revenue
    head(30)
  
  directors <- unique(collaborations$primary_director)
  actors <- unique(collaborations$primary_actor)
  
  nodes <- data.frame(
    name = c(directors, actors),
    group = c(rep("Director", length(directors)), 
              rep("Actor", length(actors))),
    size = c(rep(20, length(directors)), rep(15, length(actors)))
  )
  
  links <- collaborations %>%
    mutate(
      source = match(primary_director, nodes$name) - 1,
      target = match(primary_actor, nodes$name) - 1,
      value = collaborations * 3
    )
  
  network <- forceNetwork(
    Links = links, 
    Nodes = nodes,
    Source = "source", 
    Target = "target",
    Value = "value", 
    NodeID = "name",
    Group = "group",
    Nodesize = "size",
    opacity = 0.9,
    zoom = TRUE,
    legend = TRUE,
    fontSize = 14,
    fontFamily = "Arial",
    linkDistance = 100,
    charge = -300,
    bounded = TRUE,
    opacityNoHover = 0.7,
    colourScale = JS('d3.scaleOrdinal().domain(["Director", "Actor"]).range(["#FF6B6B", "#4ECDC4"])')
  )
  
  return(network)
}

# 8. GENRE COMBINATIONS ANALYSIS - Using adjusted values
create_genre_combinations <- function(data) {
  genre_combos <- data %>%
    filter(str_detect(Genre, ", ")) %>%
    mutate(
      genre_count = str_count(Genre, ", ") + 1,
      primary_genre = str_split(Genre, ", ", simplify = TRUE)[,1],
      secondary_genre = str_split(Genre, ", ", simplify = TRUE)[,2]
    ) %>%
    filter(!is.na(secondary_genre), secondary_genre != "") %>%
    group_by(primary_genre, secondary_genre) %>%
    summarise(
      count = n(),
      avg_revenue = mean(BoxOffice_adjusted, na.rm = TRUE),  # Using adjusted
      avg_rating = mean(imdbRating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(count >= 5)
  
  heatmap <- ggplot(genre_combos, aes(x = primary_genre, y = secondary_genre)) +
    geom_tile(aes(fill = avg_revenue/1e6)) +
    geom_text(aes(label = round(avg_rating, 1)), size = 3) +
    scale_fill_viridis(name = "Avg Revenue\n(2024 Million $)") +
    labs(title = "Genre Combination Success Matrix (Inflation-Adjusted)",
         subtitle = "Color = Average Revenue (2024 dollars) | Numbers = Average Rating",
         x = "Primary Genre", y = "Secondary Genre") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(heatmap)
}

# 11. TIME SERIES ANALYSIS - Using adjusted values
create_time_series_analysis <- function(data) {
  # Aggregate by year
  yearly_stats <- data %>%
    group_by(Year) %>%
    summarise(
      total_revenue = sum(BoxOffice_adjusted, na.rm = TRUE)/1e9,  # Using adjusted
      avg_revenue = mean(BoxOffice_adjusted, na.rm = TRUE)/1e6,   # Using adjusted
      movie_count = n(),
      avg_rating = mean(imdbRating, na.rm = TRUE),
      top_movie = first(Title[order(BoxOffice_adjusted, decreasing = TRUE)]),  # Sort by adjusted
      top_revenue = max(BoxOffice_adjusted, na.rm = TRUE)/1e6,  # Using adjusted
      .groups = "drop"
    ) %>%
    filter(Year >= 1980)  # Focus on modern era
  
  # Create interactive time series
  time_series_plot <- plot_ly(yearly_stats) %>%
    add_trace(
      x = ~Year,
      y = ~total_revenue,
      type = "scatter",
      mode = "lines+markers",
      name = "Total Revenue (2024 adj.)",
      line = list(color = "darkblue", width = 3),
      marker = list(size = 8),
      text = ~paste("Year:", Year, "<br>",
                    "Total Revenue: $", round(total_revenue, 2), "B<br>",
                    "Top Movie: ", top_movie, "<br>",
                    "Top Revenue: $", round(top_revenue, 1), "M"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    add_trace(
      x = ~Year,
      y = ~movie_count/100,  # Scale for visibility
      type = "scatter",
      mode = "lines+markers",
      name = "Movie Count (÷100)",
      yaxis = "y2",
      line = list(color = "darkgreen", width = 2),
      marker = list(size = 6)
    ) %>%
    add_trace(
      x = ~Year,
      y = ~avg_rating,
      type = "scatter",
      mode = "lines+markers",
      name = "Avg Rating",
      yaxis = "y3",
      line = list(color = "darkred", width = 2),
      marker = list(size = 6)
    ) %>%
    layout(
      title = "Movie Industry Trends Over Time (Inflation-Adjusted)",
      xaxis = list(title = "Year"),
      yaxis = list(
        title = "Total Revenue (Billion USD, 2024 adj.)",
        side = "left"
      ),
      yaxis2 = list(
        title = "Movie Count (÷100)",
        overlaying = "y",
        side = "right",
        showgrid = FALSE
      ),
      yaxis3 = list(
        title = "Average Rating",
        overlaying = "y",
        side = "right",
        position = 0.95,
        showgrid = FALSE
      ),
      hovermode = "x unified"
    )
  
  return(time_series_plot)
}

# 12. TOP PERFORMERS ANALYSIS - Using adjusted values
create_top_performers <- function(data) {
  # Top movies by adjusted revenue
  top_revenue <- data %>%
    arrange(desc(BoxOffice_adjusted)) %>%  # Using adjusted
    head(20) %>%
    mutate(rank = row_number())
  
  # Top movies by rating (with minimum votes)
  top_rated <- data %>%
    filter(imdbVotes > 50000) %>%  # Ensure popularity
    arrange(desc(imdbRating)) %>%
    head(20) %>%
    mutate(rank = row_number())
  
  # Create lollipop chart for revenue
  revenue_lollipop <- ggplot(top_revenue, aes(x = reorder(Title, BoxOffice_adjusted), 
                                              y = BoxOffice_adjusted/1e6)) +  # Using adjusted
    geom_segment(aes(xend = Title, yend = 0), color = "gray70", size = 1) +
    geom_point(aes(color = imdbRating), size = 4) +
    coord_flip() +
    scale_color_viridis(name = "IMDb Rating") +
    labs(title = "Top 20 Movies by Box Office Revenue (Inflation-Adjusted)",
         subtitle = "All values in 2024 dollars",
         x = "", y = "Box Office (Million USD)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())
  
  # Create rating comparison
  rating_comparison <- plot_ly() %>%
    add_trace(
      data = top_rated,
      x = ~imdbRating,
      y = ~reorder(Title, imdbRating),
      type = "bar",
      orientation = "h",
      marker = list(color = ~log10(imdbVotes), colorscale = "Viridis"),
      text = ~paste("Votes:", comma(imdbVotes)),
      name = "Rating"
    ) %>%
    layout(
      title = "Top 20 Highest Rated Movies",
      xaxis = list(title = "IMDb Rating"),
      yaxis = list(title = ""),
      showlegend = FALSE
    )
  
  return(list(revenue = revenue_lollipop, rating = rating_comparison))
}

# 13. INTERACTIVE DASHBOARD COMPONENTS
create_summary_boxes <- function(data) {
  stats <- create_overview_cards(data)
  
  # Create HTML boxes for flexdashboard
  boxes <- list(
    total_movies = div(
      class = "value-box",
      div(class = "value", stats$total_movies),
      div(class = "caption", "Total Movies")
    ),
    total_revenue = div(
      class = "value-box",
      div(class = "value", stats$total_revenue),
      div(class = "caption", "Total Box Office")
    ),
    avg_rating = div(
      class = "value-box",
      div(class = "value", paste0(stats$avg_rating, " ★")),
      div(class = "caption", "Average Rating")
    ),
    date_range = div(
      class = "value-box",
      div(class = "value", stats$date_range),
      div(class = "caption", "Year Range")
    )
  )
  
  return(boxes)
}

# 14. EXPORT FUNCTIONS
save_all_visualizations <- function(data, output_dir = "data_explorer_outputs/") {
  # Create directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Generate all visualizations
  cat("Generating visualizations...\n")
  
  # Save static plots
  ggsave(paste0(output_dir, "genre_treemap.png"), 
         create_genre_treemap(data), 
         width = 12, height = 8)
  
  ggsave(paste0(output_dir, "rating_evolution.png"), 
         create_rating_evolution(data), 
         width = 10, height = 8)
  
  ggsave(paste0(output_dir, "seasonal_patterns.png"), 
         create_seasonal_analysis(data), 
         width = 10, height = 10)
  
  ggsave(paste0(output_dir, "genre_combinations.png"), 
         create_genre_combinations(data), 
         width = 12, height = 10)
  
  # Save interactive plots as HTML
  htmlwidgets::saveWidget(
    create_collaboration_network(data),
    paste0(output_dir, "collaboration_network.html")
  )
  
  htmlwidgets::saveWidget(
    create_time_series_analysis(data),
    paste0(output_dir, "time_series.html")
  )
  
  cat("All visualizations saved to", output_dir, "\n")
}

# 15. MAIN EXECUTION FUNCTION
run_data_explorer <- function(data_path = "movies_clean.rds") {
  # Load data - check for RDS first, then CSV
  cat("Loading data...\n")
  if (grepl("\\.rds$", data_path)) {
    data <- readRDS(data_path)
  } else if (grepl("\\.csv$", data_path)) {
    data <- read_csv(data_path)
  } else {
    stop("Data file must be .rds or .csv format")
  }
  
  # Generate all components
  components <- list(
    overview = create_overview_cards(data),
    data_table = create_data_table(data),
    genre_treemap = create_genre_treemap(data),
    box_office_dist = create_box_office_distribution(data),
    rating_evolution = create_rating_evolution(data),
    seasonal = create_seasonal_analysis(data),
    network = create_collaboration_network(data),
    genre_combos = create_genre_combinations(data),
    time_series = create_time_series_analysis(data),
    top_performers = create_top_performers(data)
  )
  
  # Save outputs
  save_all_visualizations(data)
  
  return(components)
}

# Additional interactive visualizations

# Create interactive sankey diagram - Using adjusted values
create_genre_flow_diagram <- function(data) {
  # Prepare flow data
  genre_flow <- data %>%
    mutate(
      decade = paste0(floor(Year/10)*10, "s"),
      primary_genre = str_split(Genre, ", ", simplify = TRUE)[,1]
    ) %>%
    filter(!is.na(decade), !is.na(primary_genre)) %>%
    group_by(decade, primary_genre) %>%
    summarise(
      count = n(),
      revenue = sum(BoxOffice_adjusted, na.rm = TRUE) / 1e9,  # Using adjusted
      .groups = "drop"
    ) %>%
    filter(count >= 5)
  
  # Create nodes
  decades <- unique(genre_flow$decade)
  genres <- unique(genre_flow$primary_genre)
  
  nodes <- data.frame(
    name = c(decades, genres),
    stringsAsFactors = FALSE
  )
  
  # Create links
  links <- genre_flow %>%
    mutate(
      source = match(decade, nodes$name) - 1,
      target = match(primary_genre, nodes$name) - 1,
      value = revenue
    )
  
  # Create sankey diagram
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes$name,
      color = c(rep("lightblue", length(decades)), 
                rep("lightcoral", length(genres))),
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$value,
      label = paste("$", round(links$value, 1), "B"),
      color = "rgba(0,0,0,0.2)"
    )
  ) %>%
    layout(
      title = "Genre Revenue Flow by Decade (Inflation-Adjusted)",
      font = list(size = 12)
    )
  
  return(fig)
}

# Create interactive heatmap - Using adjusted values
create_interactive_heatmap <- function(data) {
  # Create month-genre matrix
  heatmap_data <- data %>%
    filter(!is.na(Released)) %>%
    mutate(
      month = month(Released, label = TRUE),
      primary_genre = str_split(Genre, ", ", simplify = TRUE)[,1]
    ) %>%
    group_by(month, primary_genre) %>%
    summarise(
      avg_revenue = mean(BoxOffice_adjusted, na.rm = TRUE) / 1e6,  # Using adjusted
      movie_count = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(month), !is.na(primary_genre))
  
  # Create interactive heatmap
  fig <- plot_ly(
    data = heatmap_data,
    x = ~month,
    y = ~primary_genre,
    z = ~avg_revenue,
    type = "heatmap",
    colorscale = "Viridis",
    hovertemplate = paste(
      "<b>%{y} in %{x}</b><br>",
      "Avg Revenue: $%{z:.1f}M (2024 adj.)<br>",
      "Movies: %{customdata}<br>",
      "<extra></extra>"
    ),
    customdata = ~movie_count
  ) %>%
    layout(
      title = "Genre Performance by Release Month (Inflation-Adjusted)",
      xaxis = list(title = "Release Month"),
      yaxis = list(title = "Genre"),
      annotations = list(
        text = "Average Revenue (Million $, 2024 adj.)",
        showarrow = FALSE,
        x = 1.1,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        textangle = 90
      )
    )
  
  return(fig)
}

# Create 3D interactive scatter plot - Using adjusted values
create_3d_movie_space <- function(data) {
  # Prepare data
  plot_data <- data %>%
    filter(!is.na(BoxOffice_adjusted), !is.na(imdbRating), !is.na(Runtime_num)) %>%
    sample_n(min(500, nrow(.))) %>%  # Limit data points for performance
    mutate(
      hover_text = paste(
        "<b>", Title, "</b><br>",
        "Revenue: $", format(BoxOffice_adjusted, big.mark = ","), " (2024 adj.)<br>",
        "Rating: ", imdbRating, "<br>",
        "Runtime: ", Runtime_num, " min<br>",
        "Genre: ", Genre
      )
    )
  
  # Create 3D scatter plot
  fig <- plot_ly(
    plot_data,
    x = ~imdbRating,
    y = ~log10(BoxOffice_adjusted),  # Using adjusted
    z = ~Runtime_num,
    color = ~Genre,
    text = ~hover_text,
    hovertemplate = "%{text}<extra></extra>",
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 5,
      opacity = 0.8,
      line = list(width = 0.5, color = 'white')
    )
  ) %>%
    layout(
      title = "3D Movie Space Explorer (Inflation-Adjusted)",
      scene = list(
        xaxis = list(title = "IMDb Rating"),
        yaxis = list(title = "Log(Box Office, 2024 adj.)"),
        zaxis = list(title = "Runtime (minutes)"),
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.5)
        )
      ),
      showlegend = TRUE
    )
  
  return(fig)
}


# Print completion message
cat("\nData Explorer module loaded successfully!\n")
cat("Run 'components <- run_data_explorer(\"movies_clean.rds\")' to generate all visualizations.\n")

# Create switchable timeline - Using adjusted values
create_switchable_timeline <- function(data) {
  # Prepare data with actual columns
  yearly_data <- data %>%
    group_by(Year) %>%
    summarise(
      Movies = n(),
      Revenue = sum(BoxOffice_adjusted, na.rm = TRUE) / 1e9,  # Using adjusted
      `Avg Rating` = mean(imdbRating, na.rm = TRUE),
      `Avg Runtime` = mean(Runtime_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(Year >= 1990) %>%
    pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value")
  
  # Create chart
  fig <- plot_ly(yearly_data, x = ~Year, y = ~Value, color = ~Metric) %>%
    add_lines(
      transforms = list(
        list(
          type = 'filter',
          target = ~Metric,
          operation = '=',
          value = 'Revenue'
        )
      )
    ) %>%
    layout(
      title = "Movie Industry Metrics Over Time (Inflation-Adjusted)",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Value"),
      updatemenus = list(
        list(
          type = "dropdown",
          x = 0.1,
          y = 1.15,
          buttons = list(
            list(
              method = "restyle",
              args = list("transforms[0].value", "Revenue"),
              label = "💰 Revenue (Billion $, 2024 adj.)"
            ),
            list(
              method = "restyle",
              args = list("transforms[0].value", "Movies"),
              label = "🎬 Movie Count"
            ),
            list(
              method = "restyle",
              args = list("transforms[0].value", "Avg Rating"),
              label = "⭐ Average Rating"
            ),
            list(
              method = "restyle",
              args = list("transforms[0].value", "Avg Runtime"),
              label = "⏱️ Avg Runtime (minutes)"
            )
          )
        )
      )
    )
  
  return(fig)
}

# CSS for styling (to be included in flexdashboard)
css_styles <- "
.value-box {
  background-color: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 0.25rem;
  padding: 1rem;
  text-align: center;
  margin-bottom: 1rem;
}

.value-box .value {
  font-size: 2rem;
  font-weight: bold;
  color: #495057;
}

.value-box .caption {
  font-size: 0.875rem;
  color: #6c757d;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.chart-container {
  background-color: white;
  border-radius: 0.25rem;
  box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);
  padding: 1rem;
  margin-bottom: 1rem;
}

.info-text {
  font-size: 0.9rem;
  color: #6c757d;
  font-style: italic;
}

.metric-card {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  padding: 1.5rem;
  border-radius: 0.5rem;
  margin-bottom: 1rem;
}
"


# Compare nominal vs adjusted revenue
create_inflation_comparison <- function(data) {
  comparison_data <- data %>%
    select(Title, Year, BoxOffice_num, BoxOffice_adjusted, inflation_multiplier) %>%
    arrange(desc(BoxOffice_adjusted - BoxOffice_num)) %>%
    head(20) %>%
    mutate(
      difference = BoxOffice_adjusted - BoxOffice_num,
      pct_increase = ((BoxOffice_adjusted - BoxOffice_num) / BoxOffice_num) * 100
    )
  
  p <- ggplot(comparison_data, aes(x = reorder(Title, difference))) +
    geom_segment(aes(xend = Title, y = BoxOffice_num/1e6, yend = BoxOffice_adjusted/1e6),
                 color = "gray50", size = 1) +
    geom_point(aes(y = BoxOffice_num/1e6), color = "#FF6B6B", size = 3) +
    geom_point(aes(y = BoxOffice_adjusted/1e6), color = "#4ECDC4", size = 3) +
    coord_flip() +
    labs(title = "Impact of Inflation Adjustment on Box Office Revenue",
         subtitle = "Red = Nominal, Blue = 2024 Adjusted",
         x = "",
         y = "Box Office (Million USD)") +
    theme_minimal()
  
  return(p)
}

# Summary statistics function
generate_summary_stats <- function(data) {
  stats <- list(
    total_movies = nrow(data),
    year_range = paste(min(data$Year), "-", max(data$Year)),
    total_revenue_nominal = sum(data$BoxOffice_num, na.rm = TRUE),
    total_revenue_adjusted = sum(data$BoxOffice_adjusted, na.rm = TRUE),
    avg_inflation_factor = mean(data$inflation_multiplier, na.rm = TRUE),
    genres_analyzed = n_distinct(unlist(str_split(data$Genre, ", "))),
    directors_analyzed = n_distinct(unlist(str_split(data$Director, ", "))),
    avg_rating = mean(data$imdbRating, na.rm = TRUE),
    median_revenue_adjusted = median(data$BoxOffice_adjusted, na.rm = TRUE),
    top_grossing_movie = data$Title[which.max(data$BoxOffice_adjusted)],
    top_rated_movie = data$Title[which.max(data$imdbRating)]
  )
  
  return(stats)
}