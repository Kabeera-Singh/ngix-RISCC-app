rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# Load required libraries
library(sf)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leafpop)
library(arrow)
library(DBI)
library(duckdb)

# Configuration settings - adjust these as needed
CHUNK_SIZE_DEFAULT <- 5000        # Default chunk size for data streaming
MAP_CHUNK_SIZE <- 7000            # Chunk size for map loading
DOWNLOAD_CHUNK_SIZE <- 7000       # Chunk size for downloads
SPECIES_MAP_CHUNK_SIZE <- 7000    # Chunk size for species occurrence map loading
SPECIES_DISPLAY_LIMIT <- 2000000     # Max species occurrences to display on map
FILTERED_PLOTS_LIMIT <- 10000000     # Max filtered plots to process for species comparison

# Load climate data and interpolation function
climate_lookup <- read.csv("data/climate_lookup_table.csv")
interpolate_climate <- readRDS("data/interpolate_climate_function.rds")

# Create DuckDB connection for efficient data querying
con <- dbConnect(duckdb::duckdb())

# Register the parquet file as a virtual table
dbExecute(con, "CREATE VIEW plant_data AS SELECT * FROM read_parquet('data/dater4tool_cleaned.parquet')")

# Create materialized views for common queries
dbExecute(con, "CREATE VIEW species_summary AS 
           SELECT AcceptedTaxonName, habit, COUNT(*) as total_occurrences,
                  AVG(def) as avg_def, AVG(tmax) as avg_tmax
           FROM plant_data 
           GROUP BY AcceptedTaxonName, habit")

# Get unique growth habits for UI
habits <- dbGetQuery(con, "SELECT DISTINCT habit FROM plant_data ORDER BY habit")$habit

# Helper function to build climate filter query
build_climate_filter <- function(scenario, filter_type, climate_data) {
  if (filter_type == "climatic water deficit") {
    if (scenario == "contemporary") {
      val <- climate_data$contemporary[2]
    } else if (scenario == "low (+2C)") {
      val <- climate_data$low[2]
    } else {
      val <- climate_data$med[2]
    }
    return(paste0("def >= ", val - 2.5, " AND def <= ", val + 2.5))
  } else {
    if (scenario == "contemporary") {
      val <- climate_data$contemporary[1]
    } else if (scenario == "low (+2C)") {
      val <- climate_data$low[1]
    } else {
      val <- climate_data$med[1]
    }
    return(paste0("tmax >= ", val - 0.5, " AND tmax <= ", val + 0.5))
  }
}

# Streaming data function - process in chunks to manage memory
get_filtered_data_stream <- function(scenario, filter_type, climate_data, selected_habits, chunk_size = CHUNK_SIZE_DEFAULT) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  where_clause <- paste(climate_filter, "AND", habit_filter)
  
  # Get total count
  count_query <- paste("SELECT COUNT(*) as total FROM plant_data WHERE", where_clause)
  total_rows <- dbGetQuery(con, count_query)$total
  
  if (total_rows == 0) return(data.frame())
  
  # Return streaming function instead of all data at once
  return(list(
    total_rows = total_rows,
    get_chunk = function(offset = 0, limit = chunk_size) {
      query <- paste("SELECT Plot, Long, Lat, def, tmax FROM plant_data WHERE", 
                     where_clause, "LIMIT", limit, "OFFSET", offset)
      dbGetQuery(con, query)
    }
  ))
}

# Memory-efficient species counts using aggregation
get_species_counts <- function(scenario, filter_type, climate_data, selected_habits, top_n = 30) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  where_clause <- paste(climate_filter, "AND", habit_filter)
  
  query <- paste("SELECT AcceptedTaxonName, COUNT(*) as n FROM plant_data WHERE", 
                where_clause, "GROUP BY AcceptedTaxonName ORDER BY n DESC LIMIT", top_n)
  
  return(dbGetQuery(con, query))
}

# Spatial indexing for species occurrences
get_species_occurrences_spatial <- function(species_name, bbox = NULL) {
  base_query <- paste0("SELECT Plot, Long, Lat, AcceptedTaxonName, PctCov_100 FROM plant_data WHERE AcceptedTaxonName = '", species_name, "'")
  
  # Add bounding box filter if provided
  if (!is.null(bbox)) {
    spatial_filter <- paste0(" AND Long >= ", bbox$west, " AND Long <= ", bbox$east, 
                            " AND Lat >= ", bbox$south, " AND Lat <= ", bbox$north)
    base_query <- paste0(base_query, spatial_filter)
  }
  
  return(dbGetQuery(con, base_query))
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
      rel = "stylesheet"
    )
  ),
  
  div(class = "app-header",
    div(class = "header-container",
      h1("Climate Adjusted Provenancing Tool"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),
  
  div(class = "main-container",
    div(class = "info-card",
      h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
      p(paste(
        "Welcome to the Climate Adjusted Provenancing Tool. You can use this tool to identify",
        "vegetation assemblages that correspond to current and future climate conditions at your",
        "location of interest and identify taxa that are widely distributed under these climate conditions.",
        "The data underlying this product come from Petri et al. 2022, and can be accessed in full at",
        "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3947.",
        "Follow the numeric guides below to explore the data. Metadata and additional information about this tool can be found here."
      ))
    ),
    
    div(class = "main-layout",
      div(class = "sidebar",
        div(class = "filter-card",
          div(class = "filter-header",
            h5(tags$i(class = "fas fa-sliders-h"), " Climate & Location Settings")
          ),
          div(class = "filter-body",
            # Step 1: Coordinates
            div(class = "form-group step-section",
              div(class = "step-header",
                tags$span(class = "step-number", "1"),
                tags$label(tags$i(class = "fas fa-map-marker-alt"), " Input coordinates of your focal site")
              ),
              div(class = "coordinate-inputs",
                div(class = "coord-input-group",
                  tags$label("Longitude", class = "coord-label"),
                  numericInput("Long", label = NULL, value = -72.5, step = 0.1, width = "100%")
                ),
                div(class = "coord-input-group",
                  tags$label("Latitude", class = "coord-label"),
                  numericInput("Lat", label = NULL, value = 42.4, step = 0.1, width = "100%")
                )
              )
            ),
            
            # Step 2: Climate Settings
            div(class = "form-group step-section",
              div(class = "step-header",
                tags$span(class = "step-number", "2"),
                tags$label(tags$i(class = "fas fa-thermometer-half"), " Select climate projection and filter")
              ),
              selectInput("scenario", 
                label = tags$div(class = "input-label", tags$i(class = "fas fa-calendar-alt"), " Choose climate scenario"),
                choices = c("contemporary", "low (+2C)", "med (+4C)"),
                selected = "contemporary", width = "100%"),
              selectInput("filtr",
                label = tags$div(class = "input-label", tags$i(class = "fas fa-filter"), " Choose a climate filter"),
                choices = c("temperature", "climatic water deficit"),
                selected = "temperature", width = "100%")
            ),
            
            # Step 5: Growth Habits
            div(class = "form-group step-section",
              div(class = "step-header",
                tags$span(class = "step-number", "5"),
                tags$label(tags$i(class = "fas fa-seedling"), " Select plant growth habits of interest")
              ),
              div(class = "habits-container",
                checkboxGroupInput("habit", label = NULL, 
                  choices = habits, selected = habits, width = "100%")
              )
            ),
            
            # Download Buttons
            div(class = "download-section",
              tags$h6(class = "download-title", 
                tags$i(class = "fas fa-download"), " Download Data"
              ),
              div(class = "download-buttons",
                downloadButton("downloadData", "Download Map Data", 
                  class = "btn-download btn-map-data",
                  icon = icon("map")),
                downloadButton("downloadData1", "Download Species Summary", 
                  class = "btn-download btn-species-data",
                  icon = icon("chart-bar"))
              )
            )
          )
        )
      ),
      
      div(class = "main-panel",
        # Map Card
        div(class = "results-card map-card",
          div(class = "results-header",
            div(class = "step-header-main",
              tags$span(class = "step-number-main", "3"),
              h5(tags$i(class = "fas fa-globe-americas"), " Climate-Adjusted Provenancing Localities")
            )
          ),
          div(class = "map-container",
            leafletOutput("mymap", height = "400px"),
            p(class = "map-description", 
              "Map 1: Click on any points to access additional information about the vegetation plot. Large datasets are loaded progressively.")
          )
        ),
        
        # Species Analysis Card
        div(class = "results-card species-card",
          div(class = "results-header",
            div(class = "step-header-main",
              tags$span(class = "step-number-main", "4"),
              h5(tags$i(class = "fas fa-chart-bar"), " Most Common Species in Climate Range")
            )
          ),
          div(class = "plot-container",
            plotOutput("myplot", height = '400px', click = "plot_click"),
            div(class = "step-instruction",
              div(class = "step-header-main",
                tags$span(class = "step-number-main", "6"),
                tags$p(tags$i(class = "fas fa-mouse-pointer"), " Click on bars to see species occurrence range")
              ),
              textOutput("variable_name", inline = TRUE)
            )
          )
        ),
        
        # Species Map Card
        div(class = "results-card species-map-card",
          div(class = "species-map-container",
            leafletOutput("mymap2", height = "400px"),
            p(class = "map-description",
              "Map 2: The green points depict occurrences of the selected species, and the purple points represent those within the climate-adjusted range")
          )
        )
      )
    )
  ),
  
  tags$footer(class = "app-footer",
    div(class = "footer-container",
      p("Climate Adjusted Provenancing Tool - Data from Petri et al. 2022")
    )
  )
)

server <- function(input, output, session) {
  
  # Climate value lookup based on coordinates
  myval <- reactive({
    req(input$Lat, input$Long)
    
    climate_vals <- interpolate_climate(input$Long, input$Lat, climate_lookup)
    
    myvalT <- data.frame(
      contemporary = climate_vals$contemporary_tmax,
      low = climate_vals$low_tmax,
      med = climate_vals$med_tmax
    )
    
    myvalC <- data.frame(
      contemporary = climate_vals$contemporary_def,
      low = climate_vals$low_def,
      med = climate_vals$med_def
    )
    
    myvalo <- rbind(myvalT, myvalC)
    variable <- c("maximum temperature (C)", "climatic water deficit (mm)")
    myval <- cbind(variable, myvalo)
    
    return(myval)
  })
  
  # Lazy loading for map data
  map_data_stream <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_filtered_data_stream(input$scenario, input$filtr, climate_data, input$habit)
  }) %>% debounce(500)
  
  # Get species counts using database query
  filteredData2 <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_species_counts(input$scenario, input$filtr, climate_data, input$habit, 30)
  }) %>% debounce(500)
  
  # Initialize map once
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addScaleBar(position = "bottomright") %>%
      setView(lng = -79, lat = 40, zoom = 4)
  })
  
  # Progressive map loading
  observe({
    stream_data <- map_data_stream()
    
    if (is.null(stream_data) || stream_data$total_rows == 0) {
      leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lng = input$Long, lat = input$Lat)
      return()
    }
    
    # Clear existing markers
    leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng = input$Long, lat = input$Lat)
    
    # Load data in chunks without maximum limit
    chunk_size <- min(MAP_CHUNK_SIZE, stream_data$total_rows)
    total_loaded <- 0
    
    while (total_loaded < stream_data$total_rows) {
      chunk_data <- stream_data$get_chunk(total_loaded, chunk_size)
      
      if (nrow(chunk_data) == 0) break
      
      if (input$filtr == "climatic water deficit") {
        if (!"def" %in% names(chunk_data) || all(is.na(chunk_data$def))) {
          total_loaded <- total_loaded + nrow(chunk_data)
          next
        }
        
        pal <- colorNumeric(
          palette = "inferno",
          domain = chunk_data$def,
          na.color = "transparent"
        )
        
        leafletProxy("mymap", data = chunk_data) %>%
          addCircleMarkers(
            lng = ~Long,
            lat = ~Lat,
            layerId = ~Plot,
            color = ~ pal(def),
            popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                           "<strong> Latitude: </strong>", Lat, "<br>",
                           "<strong> Longitude: </strong>", Long, "<br>"),
            radius = 2
          )
        
        if (total_loaded == 0) {  # Add legend only once
          leafletProxy("mymap") %>%
            addLegend(
              pal = pal, 
              values = chunk_data$def,
              title = "Climatic Water Deficit (mm)",
              position = "bottomleft"
            )
        }
        
      } else if (input$filtr == "temperature") { 
        if (!"tmax" %in% names(chunk_data) || all(is.na(chunk_data$tmax))) {
          total_loaded <- total_loaded + nrow(chunk_data)
          next
        }
        
        pal <- colorNumeric(
          palette = "magma",
          domain = chunk_data$tmax,
          na.color = "transparent"
        )
        
        leafletProxy("mymap", data = chunk_data) %>%
          addCircleMarkers(
            lng = ~Long,
            lat = ~Lat,
            layerId = ~Plot,
            color = ~ pal(tmax),
            popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                           "<strong> Latitude: </strong>", Lat, "<br>",
                           "<strong> Longitude: </strong>", Long, "<br>"),
            radius = 2
          )
        
        if (total_loaded == 0) {  # Add legend only once
          leafletProxy("mymap") %>%
            addLegend(
              pal = pal, 
              values = chunk_data$tmax,
              title = "Max Temperature (°C)",
              position = "bottomleft"
            )
        }
      }
      
      total_loaded <- total_loaded + nrow(chunk_data)
      gc() # Force garbage collection after each chunk
    }
    
    # Show notification for large datasets
    if (stream_data$total_rows > 50000) {
      showNotification(paste("Loaded", stream_data$total_rows, "points - this may affect performance"), 
                       type = "message", duration = 5)
    }
  })
  
  # Streaming downloads
  output$downloadData <- downloadHandler(
    filename = function() {"plotsinclimate.csv"}, 
    content = function(fname) {
      stream_data <- map_data_stream()
      
      if (is.null(stream_data) || stream_data$total_rows == 0) {
        write.csv(data.frame(), fname, row.names = FALSE)
        return()
      }
      
      # Write header
      first_chunk <- stream_data$get_chunk(0, 1000)
      write.csv(first_chunk, fname, row.names = FALSE)
      
      # Append remaining data in chunks
      total_written <- nrow(first_chunk)
      
      while (total_written < stream_data$total_rows) {
        chunk_data <- stream_data$get_chunk(total_written, DOWNLOAD_CHUNK_SIZE)
        if (nrow(chunk_data) == 0) break
        
        write.table(chunk_data, fname, sep = ",", append = TRUE, 
                   row.names = FALSE, col.names = FALSE)
        total_written <- total_written + nrow(chunk_data)
        gc() # Force garbage collection
      }
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {"commonspecies.csv"}, 
    content = function(fname) {
      species_data <- filteredData2()
      write.csv(species_data, fname, row.names = FALSE)
    }
  )
  
  # Create species abundance plot with color gradient
  output$myplot <- renderPlot({ 
    species_data <- filteredData2()
    if (nrow(species_data) == 0) return()
    
    # Add row numbers to match the plot order (top to bottom)
    species_data$plot_order <- nrow(species_data):1
    
    ggplot(species_data, aes(y = reorder(AcceptedTaxonName, n), x = n, fill = n)) +
      geom_col() +
      scale_fill_viridis_c(name = "Count", option = "plasma") +
      theme_minimal(base_size = 18) +
      ylab("") + 
      xlab("Number of Occurrences") +
      theme(axis.text.y = element_text(face = "italic")) 
  })
  
  # Use reactive value to store selected species
  selected_species <- reactiveVal(NULL)
  
  # Handle plot clicks with spatial filtering
  observeEvent(input$plot_click, {
    species_data <- filteredData2()
    if (nrow(species_data) == 0) return()
    
    click_y <- input$plot_click$y
    # Fix the order - since we reorder by n (ascending), we need to reverse the index
    clicked_variable <- nrow(species_data) - round(click_y) + 1
    
    if (clicked_variable > 0 && clicked_variable <= nrow(species_data)) {
      # Get species ordered by count (descending) to match plot display
      species_ordered <- species_data[order(species_data$n, decreasing = TRUE), ]
      new_species <- species_ordered$AcceptedTaxonName[clicked_variable]
      
      if (is.null(selected_species()) || selected_species() != new_species) {
        selected_species(new_species)
        
        output$variable_name <- renderText({
          paste("Focal species:", new_species)
        })
        
        # Initialize map 2
        output$mymap2 <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addScaleBar(position = "bottomright") %>%
            setView(lng = -79, lat = 40, zoom = 4)
        })
        
        # Get current map bounds for spatial filtering
        bbox <- list(west = -125, east = -65, south = 25, north = 50)  # Continental US bounds
        
        # Get species occurrence data with spatial filtering
        species_occurrences <- get_species_occurrences_spatial(new_species, bbox)
        
        # Get current filtered plots for comparison
        stream_data <- map_data_stream()
        if (!is.null(stream_data) && stream_data$total_rows > 0) {
          # Load filtered plots in chunks and collect Plot IDs
          filtered_plots <- c()
          total_loaded <- 0
          
          while (total_loaded < min(stream_data$total_rows, FILTERED_PLOTS_LIMIT)) {
            chunk_data <- stream_data$get_chunk(total_loaded, 2000)
            if (nrow(chunk_data) == 0) break
            
            filtered_plots <- c(filtered_plots, chunk_data$Plot)
            total_loaded <- total_loaded + nrow(chunk_data)
            gc()  # Clean up after each chunk
          }
          
          climate_matched <- species_occurrences[species_occurrences$Plot %in% filtered_plots, ]
        } else {
          climate_matched <- data.frame()
        }
        
        # Load species occurrences in chunks for better performance
        total_species_points <- nrow(species_occurrences)
        
        if (total_species_points > SPECIES_DISPLAY_LIMIT) {
          # Sample the data if it exceeds display limit
          species_occurrences <- species_occurrences[sample(nrow(species_occurrences), SPECIES_DISPLAY_LIMIT), ]
          showNotification(paste("Showing sample of", SPECIES_DISPLAY_LIMIT, "of", total_species_points, "species occurrences"), 
                          type = "message", duration = 3)
        }
        
        # Clear existing markers
        leafletProxy("mymap2") %>%
          clearShapes() %>%
          clearMarkers()
        
        # Load species occurrences in chunks
        chunk_size <- min(SPECIES_MAP_CHUNK_SIZE, nrow(species_occurrences))
        total_loaded_species <- 0
        
        while (total_loaded_species < nrow(species_occurrences)) {
          end_idx <- min(total_loaded_species + chunk_size, nrow(species_occurrences))
          chunk_species <- species_occurrences[(total_loaded_species + 1):end_idx, ]
          
          # Add all occurrences (green)
          leafletProxy("mymap2", data = chunk_species) %>%
            addCircles(
              color = "green",
              lng = ~Long,
              lat = ~Lat, 
              radius = 4,
              popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                             "<strong> Latitude: </strong>", Lat, "<br>",
                             "<strong> Longitude: </strong>", Long, "<br>",
                             "<strong> Species </strong>", AcceptedTaxonName, "<br>",
                             "<strong> Relative Cover </strong>", paste(PctCov_100, "%"), "<br>")
            )
          
          total_loaded_species <- end_idx
          gc() # Force garbage collection after each chunk
        }
        
        # Add climate-matched occurrences in chunks (purple)
        if (nrow(climate_matched) > 0) {
          chunk_size_matched <- min(SPECIES_MAP_CHUNK_SIZE, nrow(climate_matched))
          total_loaded_matched <- 0
          
          while (total_loaded_matched < nrow(climate_matched)) {
            end_idx <- min(total_loaded_matched + chunk_size_matched, nrow(climate_matched))
            chunk_matched <- climate_matched[(total_loaded_matched + 1):end_idx, ]
            
            leafletProxy("mymap2", data = chunk_matched) %>%
              addCircleMarkers(
                color = "purple",
                lng = ~Long,
                lat = ~Lat, 
                radius = 3,
                popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                               "<strong> Latitude: </strong>", Lat, "<br>",
                               "<strong> Longitude: </strong>", Long, "<br>",
                               "<strong> Species </strong>", AcceptedTaxonName, "<br>",
                               "<strong> Relative Cover </strong>", paste(PctCov_100, "%"), "<br>")
              )
            
            total_loaded_matched <- end_idx
            gc() # Force garbage collection after each chunk
          }
        }
        
        # Show notification for large datasets
        if (total_species_points > 10000) {
          showNotification(paste("Loaded", min(total_species_points, SPECIES_DISPLAY_LIMIT), "species occurrences"), 
                           type = "message", duration = 3)
        }
        
        gc() # Final cleanup
      }
    }
  })
  
  # Clean up database connection when app stops
  onStop(function() {
    dbDisconnect(con)
    gc()
  })
}

# Run the app
shinyApp(ui = ui, server = server)