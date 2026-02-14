# ============================================================================
# Climate Adjusted Provenancing Tool
# ============================================================================
# 
# A Shiny application for identifying vegetation assemblages that correspond 
# to current and future climate conditions at locations of interest.
# ============================================================================

# Environment Setup -----------------------------------------------------------
options(stringsAsFactors = FALSE)

# Load Required Libraries -----------------------------------------------------
library(sf)         # Spatial data handling
library(shiny)      # Web application framework
library(leaflet)    # Interactive maps
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(leafpop)    # Leaflet popups
library(arrow)      # Parquet file reading
library(DBI)        # Database interface
library(duckdb)     # In-memory analytical database
library(data.table) # Fast CSV loading

# Configuration Parameters ----------------------------------------------------
CHUNK_SIZE_DEFAULT <- 5000        # Default chunk size for data streaming
MAP_CHUNK_SIZE <- 7000            # Chunk size for map loading
DOWNLOAD_CHUNK_SIZE <- 7000       # Chunk size for downloads
SPECIES_MAP_CHUNK_SIZE <- 7000    # Chunk size for species occurrence map loading
SPECIES_DISPLAY_LIMIT <- 2000000  # Maximum species occurrences to display on map
FILTERED_PLOTS_LIMIT <- 10000000  # Maximum filtered plots to process for species comparison

# Data Initialization ---------------------------------------------------------
# Load climate data and interpolation function (fread is faster than read_csv)
climate_lookup <- as.data.frame(data.table::fread("data/climate_lookup_table.csv"))
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

# Prepare growth habits data for UI
habits_raw <- dbGetQuery(con, "SELECT DISTINCT habit FROM plant_data ORDER BY habit")$habit
habits <- habits_raw[habits_raw != "" & !is.na(habits_raw)]
habits <- c(habits, "Other")  # Add "Other" for empty/missing habits

# Helper Functions ------------------------------------------------------------

#' Build Climate Filter Query
#' 
#' Constructs SQL WHERE clause for climate filtering based on scenario and type
#' 
#' @param scenario Climate scenario ("contemporary", "low (+2C)", "med (+4C)")
#' @param filter_type Climate variable ("temperature", "climatic water deficit")
#' @param climate_data Data frame with climate values
#' @return String SQL WHERE clause
build_climate_filter <- function(scenario, filter_type, climate_data) {
  if (filter_type == "climatic water deficit") {
    val <- switch(scenario,
                  "contemporary" = climate_data$contemporary[2],
                  "low (+2C)" = climate_data$low[2],
                  climate_data$med[2])
    return(paste0("def >= ", val - 2.5, " AND def <= ", val + 2.5))
  } else {
    val <- switch(scenario,
                  "contemporary" = climate_data$contemporary[1],
                  "low (+2C)" = climate_data$low[1],
                  climate_data$med[1])
    return(paste0("tmax >= ", val - 0.5, " AND tmax <= ", val + 0.5))
  }
}

#' Get Filtered Data Stream
#' 
#' Creates a streaming data function for memory-efficient data processing
#' 
#' @param scenario Climate scenario
#' @param filter_type Climate filter type
#' @param climate_data Climate values data
#' @param selected_habits Vector of selected growth habits
#' @param chunk_size Size of data chunks to process
#' @return List with total_rows count and get_chunk function
get_filtered_data_stream <- function(scenario, filter_type, climate_data, selected_habits, chunk_size = CHUNK_SIZE_DEFAULT) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  
  # Handle "Other" selection by including empty/null habits
  if ("Other" %in% selected_habits) {
    other_habits <- selected_habits[selected_habits != "Other"]
    if (length(other_habits) > 0) {
      habit_filter <- paste0("(habit IN ('", paste(other_habits, collapse = "', '"), "') OR habit = '' OR habit IS NULL)")
    } else {
      habit_filter <- "(habit = '' OR habit IS NULL)"
    }
  } else {
    habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  }
  
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

#' Get Species Counts
#' 
#' Retrieves species occurrence counts using efficient database aggregation
#' 
#' @param scenario Climate scenario
#' @param filter_type Climate filter type
#' @param climate_data Climate values data
#' @param selected_habits Vector of selected growth habits
#' @param top_n Number of top species to return
#' @return Data frame with species names and occurrence counts
get_species_counts <- function(scenario, filter_type, climate_data, selected_habits, top_n = 100) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  
  # Handle "Other" selection by including empty/null habits
  if ("Other" %in% selected_habits) {
    other_habits <- selected_habits[selected_habits != "Other"]
    if (length(other_habits) > 0) {
      habit_filter <- paste0("(habit IN ('", paste(other_habits, collapse = "', '"), "') OR habit = '' OR habit IS NULL)")
    } else {
      habit_filter <- "(habit = '' OR habit IS NULL)"
    }
  } else {
    habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  }
  
  where_clause <- paste(climate_filter, "AND", habit_filter)
  
  query <- paste("SELECT AcceptedTaxonName, COUNT(*) as n FROM plant_data WHERE", 
                where_clause, "GROUP BY AcceptedTaxonName ORDER BY n DESC LIMIT", top_n)
  
  return(dbGetQuery(con, query))
}

#' Get Species Occurrences with Spatial Filtering
#' 
#' Retrieves species occurrence data with optional bounding box filtering
#' 
#' @param species_name Scientific name of the species
#' @param bbox Optional bounding box list with west, east, south, north coordinates
#' @return Data frame with species occurrence data
get_species_occurrences_spatial <- function(species_name, bbox = NULL) {
  # Escape single quotes to prevent SQL injection (e.g. species names with apostrophes like "O'Brien")
  species_escaped <- gsub("'", "''", species_name)
  base_query <- paste0("SELECT Plot, Long, Lat, AcceptedTaxonName, PctCov_100 FROM plant_data WHERE AcceptedTaxonName = '", species_escaped, "'")
  
  # Add bounding box filter if provided (bbox values are numeric from code, not user input)
  if (!is.null(bbox)) {
    spatial_filter <- paste0(" AND Long >= ", bbox$west, " AND Long <= ", bbox$east,
                             " AND Lat >= ", bbox$south, " AND Lat <= ", bbox$north)
    base_query <- paste0(base_query, spatial_filter)
  }
  
  return(dbGetQuery(con, base_query))
}

# User Interface Definition ---------------------------------------------------
ui <- fluidPage(
  # Head Section: CSS and External Libraries
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
      rel = "stylesheet"
    )
  ),
  
  # Application Header
  div(class = "app-header",
    div(class = "header-container",
      h1("Climate Adjusted Provenancing Tool"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),
  
  # Main Application Content
  div(class = "main-container",
    # Information Section
div(class = "info-card",
  h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
  p(
    "Welcome to the Climate Adjusted Provenancing Tool. Use this tool to (1) map sites where vegetation composition has been measured and that correspond to the current and/or future climate conditions of your location and (2) identify species by growth form within the mapped sites. This tool can be used to inform native seed collected for restoration using a climate-adjusted provenancing approach (",
    tags$a(href = "https://www.risccnetwork.org/research-summaries/prober2016", target = "_blank", "learn more"),
    "). The data underlying this product come from ",
    tags$a(href = "https://doi.org/10.1002/ecy.3947", target = "_blank", "Petri et al. 2022"),
    "."
  )
),
    
    # Main Layout: Sidebar and Content
    div(class = "main-layout",
      # Sidebar: Controls and Filters
      div(class = "sidebar",
        div(class = "filter-card",
          div(class = "filter-header",
            h5(tags$i(class = "fas fa-sliders-h"), " Climate & Location Settings")
          ),
          div(class = "filter-body",
            # Step 1: Coordinate Input
            div(class = "form-group step-section",
              div(class = "step-header",
                tags$span(class = "step-number", "1"),
                  tags$label(
                    tags$i(class = "fas fa-map-marker-alt"), 
                    " Input your site coordinates (e.g., Amherst, MA: -72.5, 42.4). ",
                    tags$a(href = "https://www.latlong.net/", target = "_blank", "Look up coordinates here.")
                  )
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
            
            # Step 3: Growth Habits Selection
            div(class = "form-group step-section",
              div(class = "step-header",
                tags$span(class = "step-number", "3"),
                tags$label(tags$i(class = "fas fa-seedling"), " Select plant growth habits of interest")
              ),
              div(class = "habits-container",
                div(class = "habits-controls",
                  actionButton("clearAllHabits", "Clear All", class = "btn btn-sm btn-outline-secondary clear-all-btn")
                ),
                checkboxGroupInput("habit", label = NULL, 
                  choices = habits, selected = habits, width = "100%")
              )
            ),
            
            # Download Section
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
      
      # Main Panel: Results and Visualizations
      div(class = "main-panel",
        # Step 4: Interactive Map
        div(class = "results-card map-card",
          div(class = "results-header",
            div(class = "step-header-main",
              tags$span(class = "step-number-main", "4"),
              h5(tags$i(class = "fas fa-globe-americas"), " Climate-Adjusted Provenancing Localities")
            )
          ),
          div(class = "map-container",
            leafletOutput("mymap", height = "400px"),
            p(class = "map-description", 
              "Map 1: Click on any points to access additional information about the vegetation plot. Large datasets are loaded progressively.")
          )
        ),
        
        # Step 5: Species Analysis Chart
        div(class = "results-card species-card",
          div(class = "results-header",
            div(class = "step-header-main",
              tags$span(class = "step-number-main", "5"),
              h5(tags$i(class = "fas fa-chart-bar"), " Most Common Species in Climate Range")
            )
          ),
          div(class = "plot-container",
            plotOutput("myplot", height = '400px')
          )
        ),
        
        # Step 6: Species Selection and Occurrence Map
        div(class = "results-card species-selection-card",
          div(class = "results-header",
            div(class = "step-header-main",
              tags$span(class = "step-number-main", "6"),
              h5(tags$i(class = "fas fa-mouse-pointer"), " Select Species to See Occurrence Range")
            )
          ),
          div(class = "selection-container",
            selectInput("selected_species", 
              label = "Choose a species:",
              choices = NULL,
              selected = NULL,
              width = "100%"),
            textOutput("variable_name", inline = TRUE)
          )
        ,
        
        # Species Occurrence Map
        div(class = "results-card species-map-card",
          div(class = "species-map-container",
            leafletOutput("mymap2", height = "400px"),
            p(class = "map-description",
              "Map 2: The green points depict occurrences of the selected species, and the purple points represent those within the climate-adjusted range")
          )
        )
        )
      )
    )
  ),
  
  # Application Footer
  tags$footer(class = "app-footer",
    div(class = "footer-container",
      p("Climate Adjusted Provenancing Tool - Data from Petri et al. 2022")
    )
  )
)

# Server Logic Definition -----------------------------------------------------
server <- function(input, output, session) {
  
  # Event Handlers ------------------------------------------------------------
  
  # Handle Clear All button for growth habits
  observeEvent(input$clearAllHabits, {
    updateCheckboxGroupInput(session, "habit", selected = character(0))
  })
  
  # Reactive Values -----------------------------------------------------------
  
  # Climate value lookup based on user coordinates
  myval <- reactive({
    req(input$Lat, input$Long)
    
    climate_vals <- interpolate_climate(input$Long, input$Lat, climate_lookup)
    
    # Temperature values for all scenarios
    myvalT <- data.frame(
      contemporary = climate_vals$contemporary_tmax,
      low = climate_vals$low_tmax,
      med = climate_vals$med_tmax
    )
    
    # Climatic water deficit values for all scenarios
    myvalC <- data.frame(
      contemporary = climate_vals$contemporary_def,
      low = climate_vals$low_def,
      med = climate_vals$med_def
    )
    
    # Combine temperature and deficit data
    myvalo <- rbind(myvalT, myvalC)
    variable <- c("maximum temperature (C)", "climatic water deficit (mm)")
    myval <- cbind(variable, myvalo)
    
    return(myval)
  })
  
  # Filtered data stream for memory-efficient processing
  map_data_stream <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_filtered_data_stream(input$scenario, input$filtr, climate_data, input$habit)
  }) %>% debounce(500)
  
  # Species occurrence counts
  filteredData2 <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_species_counts(input$scenario, input$filtr, climate_data, input$habit, 100)
  }) %>% debounce(500)
  
  # Update species dropdown when data changes
  observe({
    species_data <- filteredData2()
    if (nrow(species_data) > 0) {
      # Create cleaner choices with just the count in parentheses
      choices <- setNames(species_data$AcceptedTaxonName, 
                         paste0(species_data$AcceptedTaxonName, " (", species_data$n, ")"))
      updateSelectInput(session, "selected_species", choices = choices, selected = NULL)
    } else {
      updateSelectInput(session, "selected_species", choices = NULL, selected = NULL)
    }
  })
  
  # Map Rendering -------------------------------------------------------------
  
  # Initialize primary map
  output$mymap <- renderLeaflet({
    leaflet(
            options = leafletOptions(minZoom = 3, maxZoom = 18)
    ) %>%
      addTiles() %>%
      addScaleBar(position = "bottomright") %>%
      setView(lng = -79, lat = 40, zoom = 4)
  })
  
  # Progressive map loading with chunked data
  observe({
    stream_data <- map_data_stream()
    
    # Handle empty data case
    if (is.null(stream_data) || stream_data$total_rows == 0) {
      leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        addMarkers(lng = input$Long, lat = input$Lat)
      return()
    }
    
    # Clear existing markers and add user location
    leafletProxy("mymap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng = input$Long, lat = input$Lat)
    
    # Load data in chunks for performance
    chunk_size <- min(MAP_CHUNK_SIZE, stream_data$total_rows)
    total_loaded <- 0
    
    while (total_loaded < stream_data$total_rows) {
      chunk_data <- stream_data$get_chunk(total_loaded, chunk_size)
      
      if (nrow(chunk_data) == 0) break
      
      # Render based on selected climate filter
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
        
        # Add legend only once
        if (total_loaded == 0) {
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
        
        # Add legend only once
        if (total_loaded == 0) {
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
    }
    if (stream_data$total_rows > 10000) gc()
    
    # Show performance notification for large datasets
    if (stream_data$total_rows > 50000) {
      showNotification(paste("Loaded", stream_data$total_rows, "points - this may affect performance"), 
                       type = "message", duration = 5)
    }
  })
  
  # Data Download Handlers ----------------------------------------------------
  
  # Download filtered plot data
  output$downloadData <- downloadHandler(
    filename = function() {"plotsinclimate.csv"}, 
    content = function(fname) {
      stream_data <- map_data_stream()
      
      if (is.null(stream_data) || stream_data$total_rows == 0) {
        write.csv(data.frame(), fname, row.names = FALSE)
        return()
      }
      
      # Write data in chunks to manage memory
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
      }
      if (stream_data$total_rows > 10000) gc()
    }
  )
  
  # Download species summary data
  output$downloadData1 <- downloadHandler(
    filename = function() {"commonspecies.csv"}, 
    content = function(fname) {
      species_data <- filteredData2()
      write.csv(species_data, fname, row.names = FALSE)
    }
  )
  
  # Plot Rendering ------------------------------------------------------------
  
  # Species abundance chart
  output$myplot <- renderPlot({ 
    species_data <- filteredData2()
    if (nrow(species_data) == 0) return()
    
    # Show only top 30 species in the plot for readability
    plot_data <- species_data[1:min(30, nrow(species_data)), ]
    
    ggplot(plot_data, aes(y = reorder(AcceptedTaxonName, n), x = n, fill = n)) +
      geom_col() +
      scale_fill_viridis_c(name = "Count", option = "plasma") +
      theme_minimal(base_size = 18) +
      ylab("") + 
      xlab("Number of Occurrences") +
      theme(axis.text.y = element_text(face = "italic")) 
  })
  
  # Species Selection and Mapping ---------------------------------------------
  
  # Handle species selection from dropdown
  observeEvent(input$selected_species, {
    if (!is.null(input$selected_species) && input$selected_species != "") {
      selected_species_name <- input$selected_species
      
      # Update focal species display
      output$variable_name <- renderText({
        paste("Focal species:", selected_species_name)
      })
      
      # Initialize species occurrence map
      output$mymap2 <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addTiles() %>%
          addScaleBar(position = "bottomright") %>%
          setView(lng = -79, lat = 40, zoom = 4)
      })
      
      # Set spatial bounds for continental US
      bbox <- list(west = -125, east = -65, south = 25, north = 50)
      
      # Get species occurrence data with spatial filtering
      species_occurrences <- get_species_occurrences_spatial(selected_species_name, bbox)
      
      # Get current filtered plots for climate matching
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
        }
        
        climate_matched <- species_occurrences[species_occurrences$Plot %in% filtered_plots, ]
      } else {
        climate_matched <- data.frame()
      }
      
      # Handle large datasets by sampling if necessary
      total_species_points <- nrow(species_occurrences)
      
      if (total_species_points > SPECIES_DISPLAY_LIMIT) {
        # Sample the data if it exceeds display limit
        species_occurrences <- species_occurrences[sample(nrow(species_occurrences), SPECIES_DISPLAY_LIMIT), ]
        showNotification(paste("Showing sample of", SPECIES_DISPLAY_LIMIT, "of", total_species_points, "species occurrences"), 
                        type = "message", duration = 3)
      }
      
      # Clear existing markers on species map
      leafletProxy("mymap2") %>%
        clearShapes() %>%
        clearMarkers()
      
      # Load species occurrences in chunks for performance
      chunk_size <- min(SPECIES_MAP_CHUNK_SIZE, nrow(species_occurrences))
      total_loaded_species <- 0
      
      while (total_loaded_species < nrow(species_occurrences)) {
        end_idx <- min(total_loaded_species + chunk_size, nrow(species_occurrences))
        chunk_species <- species_occurrences[(total_loaded_species + 1):end_idx, ]
        
        # Add all occurrences (green markers)
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
      }
      
      # Add climate-matched occurrences in chunks (purple markers)
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
        }
      }
      if (total_species_points > 10000 || nrow(climate_matched) > 10000) gc()
      
      # Show notification for large datasets
      if (total_species_points > 10000) {
        showNotification(paste("Loaded", min(total_species_points, SPECIES_DISPLAY_LIMIT), "species occurrences"), 
                         type = "message", duration = 3)
      }
    }
  })
  
  # Application Cleanup -------------------------------------------------------
  
  # Clean up database connection when app stops
  onStop(function() {
    dbDisconnect(con)
    gc()
  })
}

# Application Execution -------------------------------------------------------
# Run the Shiny application
shinyApp(ui = ui, server = server)