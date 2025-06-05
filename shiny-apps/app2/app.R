rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

# Load required libraries
library(sf)
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leafpop)
library(arrow)
library(DBI)
library(duckdb)

# Load climate data (keep this small lookup table in memory)
climate_lookup <- read.csv("data/climate_lookup_table.csv")
interpolate_climate <- readRDS("data/interpolate_climate_function.rds")

# Create DuckDB connection for efficient data querying
con <- dbConnect(duckdb::duckdb())

# Register the parquet file as a virtual table (doesn't load into memory)
dbExecute(con, "CREATE VIEW plant_data AS SELECT * FROM read_parquet('data/dater4tool_cleaned.parquet')")

# Get unique growth habits for UI (only load this small subset)
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

# Helper function to get filtered data efficiently
get_filtered_data <- function(scenario, filter_type, climate_data, selected_habits) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  
  # Build habit filter
  habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  
  # Combine filters
  where_clause <- paste(climate_filter, "AND", habit_filter)
  
  # Execute query and return only what we need
  query <- paste("SELECT Plot, Long, Lat, def, tmax FROM plant_data WHERE", where_clause)
  
  return(dbGetQuery(con, query))
}

# Helper function to get species counts
get_species_counts <- function(scenario, filter_type, climate_data, selected_habits, top_n = 30) {
  climate_filter <- build_climate_filter(scenario, filter_type, climate_data)
  habit_filter <- paste0("habit IN ('", paste(selected_habits, collapse = "', '"), "')")
  where_clause <- paste(climate_filter, "AND", habit_filter)
  
  query <- paste("SELECT AcceptedTaxonName, COUNT(*) as n FROM plant_data WHERE", 
                where_clause, "GROUP BY AcceptedTaxonName ORDER BY n DESC LIMIT", top_n)
  
  return(dbGetQuery(con, query))
}

# Helper function to get species occurrences
get_species_occurrences <- function(species_name) {
  query <- paste0("SELECT Plot, Long, Lat, AcceptedTaxonName, PctCov_100 FROM plant_data WHERE AcceptedTaxonName = '", species_name, "'")
  return(dbGetQuery(con, query))
}

ui <- page_fluid(
  titlePanel("Climate Adjusted Provenancing Tool"),
  sidebarLayout(
    sidebarPanel=sidebarPanel(width = 3,
                              card(tags$p("1) Input the coordinates of your focal site here.", style = "font-family: 'Calibri'; font-size: 16px;"),  
                                   numericInput("Long", "longitude", value = -72.5,step=0.1),
                                   numericInput("Lat", "latitude", value = 42.4,step=0.1)),
                              card(tags$p("2) Select the climate projection SCENARIO and climate variable FILTER you'd like to include in the reference site finder", style = "font-family: 'Calibri'; font-size: 16px;"),
                                   
                                   selectizeInput(inputId = 'scenario',label=" choose climate scenario",choices=c("contemporary","low (+2C)","med (+4C)"),selected="contemporary"),
                                   selectizeInput(inputId = 'filtr',label="choose a climate filter",choices=c("temperature","climatic water deficit"),selected="temperature")),
                              
                              
                              card(tags$p("5) Select the plants growth habits of interest.", style = "font-family: 'Calibri'; font-size: 16px;"),
                                   checkboxGroupInput("habit",label ="Growth Habit", choices = habits, selected = habits)),
                              card(downloadButton("downloadData", "Download data subset from map 1"),
                                   downloadButton("downloadData1", "Download table summary from plot 1"))
    ),
    mainPanel = mainPanel(
      
      card(p("Welcome to the Climate Adjusted Provenancing Tool. You can use this tool to identify vegetation assemblages that correspond to current and future climate conditions at your location of interest and identify taxa that are widely distributed under these climate conditions. The data underlying this product come from Petri et al. 2022, and can be accessed in full at https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.3947. Follow the numeric guides below to explore the data. Metadata and additional information about this tool can be found here.")),                
      card(p("Climate projections for your focal site:"),
           tableOutput("tellum"),height = '20vh'),
      card(tags$p("3) View a map of climate-adjusted, potential provenancing localities for your focal site:",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
           leafletOutput("mymap", height = '45vh'),
           p("Map 1: Click on any points to access additional information about the vegetation plot.")),
      card(
        tags$p("4) View the 30 most common species in your climate adjusted provenancing range.",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
        plotOutput("myplot",height='50vh',click = "plot_click"),
        tags$p("6) Click on the bars to see the occurrence range of individual species on the map below.",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"),
        textOutput("variable_name"),
        leafletOutput("mymap2",height='30vh'),
        tags$p("Map 2: The green points depict every occurrence of the selected species in the dataset, and the purple points represent those that are within the climate-adjusted provenancing climatic range",style = "font-family: 'Calibri'; font-face: Bold; font-size: 16px;"))
      
    ))
)

server <- function(input, output, session) {
  
  # Climate value lookup (same as before)
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
  
  output$tellum <- renderTable(myval())
  
  # Filter data using database queries (much more memory efficient)
  filteredData <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_filtered_data(input$scenario, input$filtr, climate_data, input$habit)
  }) 
  
  # Get species counts using database query
  filteredData2 <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_species_counts(input$scenario, input$filtr, climate_data, input$habit, 30)
  })  
  
  # Get larger species summary for download
  filteredDataSUM <- reactive({
    req(input$scenario, input$filtr, input$habit)
    climate_data <- myval()
    get_species_counts(input$scenario, input$filtr, climate_data, input$habit, 150)
  }) 
  
  # Make map1
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addScaleBar(position = "bottomright") %>%
      setView(lng = -79, lat = 40, zoom = 4)
  })
  
  observe({
  filtered_data <- filteredData()
  
  # Add safety check to ensure data exists and has the required columns
  if (nrow(filtered_data) == 0 || is.null(filtered_data)) {
    return()
  }
  
  if (input$filtr == "climatic water deficit") {
    # Check if 'def' column exists and has valid values
    if (!"def" %in% names(filtered_data) || all(is.na(filtered_data$def))) {
      return()
    }
    
    pal <- colorNumeric(
      palette = "inferno",
      domain = filtered_data$def,
      na.color = "transparent"  # Handle NA values
    )
    
    leafletProxy("mymap", data = filtered_data) %>%
      clearControls() %>%
      clearMarkers() %>%
      addMarkers(
        lng = input$Long,
        lat = input$Lat) %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = ~Long,
        lat = ~Lat,
        layerId = ~Plot,
        color = ~ pal(def),
        popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                       "<strong> Latitude: </strong>", Lat, "<br>",
                       "<strong> Longitude: </strong>", Long, "<br>"),
        radius = 2) %>%
      addLegend(
        pal = pal, 
        values = ~def,
        title = "Climatic Water Deficit (mm)",
        position = "bottomleft"
      )
    
  } else if (input$filtr == "temperature") { 
    # Check if 'tmax' column exists and has valid values
    if (!"tmax" %in% names(filtered_data) || all(is.na(filtered_data$tmax))) {
      return()
    }
    
    pal <- colorNumeric(
      palette = "magma",
      domain = filtered_data$tmax,
      na.color = "transparent"  # Handle NA values
    )
    
    leafletProxy("mymap", data = filtered_data) %>%
      clearControls() %>%
      clearMarkers() %>%
      addMarkers(
        lng = input$Long,
        lat = input$Lat) %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = ~Long,
        lat = ~Lat,
        layerId = ~Plot,
        color = ~ pal(tmax),
        popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                       "<strong> Latitude: </strong>", Lat, "<br>",
                       "<strong> Longitude: </strong>", Long, "<br>"),
        radius = 2) %>%
      addLegend(
        pal = pal, 
        values = ~tmax,
        title = "Max Temperature (°C)",
        position = "bottomleft"
      )
  }
})
  
  # Download handlers
  output$downloadData <- downloadHandler(
    filename = function() {"plotsinclimate.csv"}, 
    content = function(fname) {
      write.csv(filteredData(), fname, row.names = FALSE)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {"commonspecies.csv"}, 
    content = function(fname) {
      write.csv(filteredDataSUM(), fname, row.names = FALSE)
    }
  )
  
  # Make plot
  output$myplot <- renderPlot({ 
    species_data <- filteredData2()
    if (nrow(species_data) == 0) return()
    
    ggplot(species_data, aes(y = reorder(AcceptedTaxonName, n), x = n)) +
      geom_col() +
      theme_minimal(base_size = 18) +
      ylab("") + 
      theme(axis.text.y = element_text(face = "italic")) 
  })
  
  # Handle plot clicks
  observeEvent(input$plot_click, {
    species_data <- filteredData2()
    if (nrow(species_data) == 0) return()
    
    click_y <- input$plot_click$y
    clicked_variable <- round(click_y)
    
    if (clicked_variable > 0 && clicked_variable <= nrow(species_data)) {
      selected_species <- species_data$AcceptedTaxonName[clicked_variable]
      
      output$variable_name <- renderText({
        paste("Focal species:", selected_species)
      })
      
      # Make map 2
      output$mymap2 <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addScaleBar(position = "bottomright") %>%
          setView(lng = -79, lat = 40, zoom = 4)
      })
      
      # Get species occurrence data
      species_occurrences <- get_species_occurrences(selected_species)
      
      # Filter to climate-matched occurrences
      filtered_plots <- filteredData()$Plot
      climate_matched <- species_occurrences[species_occurrences$Plot %in% filtered_plots, ]
      
      # Add all occurrences (green)
      leafletProxy("mymap2", data = species_occurrences) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addCircles(color = "green",
                   lng = ~Long,
                   lat = ~Lat, radius = 4,
                   popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                                  "<strong> Latitude: </strong>", Lat, "<br>",
                                  "<strong> Longitude: </strong>", Long, "<br>",
                                  "<strong> Species </strong>", AcceptedTaxonName, "<br>",
                                  "<strong> Relative Cover </strong>", paste(PctCov_100, "%"), "<br>"))
      
      # Add climate-matched occurrences (purple)
      if (nrow(climate_matched) > 0) {
        leafletProxy("mymap2", data = climate_matched) %>%
          addCircleMarkers(color = "purple",
                           lng = ~Long,
                           lat = ~Lat, radius = 3,
                           popup = ~paste("<strong> PlotID: </strong>", Plot, "<br>",
                                          "<strong> Latitude: </strong>", Lat, "<br>",
                                          "<strong> Longitude: </strong>", Long, "<br>",
                                          "<strong> Species </strong>", AcceptedTaxonName, "<br>",
                                          "<strong> Relative Cover </strong>", paste(PctCov_100, "%"), "<br>"))
      }
    }
  })
  
  # Clean up database connection when app stops
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui = ui, server = server)